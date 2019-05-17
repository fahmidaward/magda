import "mocha";
import * as pg from "pg";
import * as _ from "lodash";
import * as chai from "chai";
import * as chaiAsPromised from "chai-as-promised";
import NestedSetModelQueryer, { NodeRecord } from "../NestedSetModelQueryer";
import getTestDBConfig from "./getTestDBConfig";

chai.use(chaiAsPromised);
const { expect } = chai;

async function checkNodeLeftRight(
    queryer: NestedSetModelQueryer,
    nodeName: string,
    expectedLeft: number,
    expectedRight: number
) {
    queryer.defaultSelectFieldList = ["id", "name", "left", "right"];
    const testNode = (await queryer.getNodesByName(nodeName))[0];
    expect(testNode.left).to.equal(expectedLeft);
    expect(testNode.right).to.equal(expectedRight);
}

describe("Test NestedSetModelQueryer", function(this: Mocha.ISuiteCallbackContext) {
    this.timeout(10000);
    let pool: pg.Pool = null;
    const createTables: string[] = [];
    const dbConfig = getTestDBConfig();

    before(async function() {
        // --- you have to supply a db name to connect to pg
        pool = new pg.Pool({ ...dbConfig });
        try {
            await pool.query("CREATE database test");
        } catch (e) {
            // --- if database `test` already there
            // --- then mute the error
            if (e.code !== "42P04") {
                throw e;
            }
        }
        // --- end the current one & create a new one
        await pool.end();
        pool = new pg.Pool({ ...dbConfig, database: "test" });
        await pool.query('CREATE EXTENSION IF NOT EXISTS "uuid-ossp"');
    });

    after(async function() {
        if (pool) {
            for (let i = 0; i < createTables.length; i++) {
                await pool.query(`DROP TABLE IF EXISTS "${createTables[i]}"`);
            }
            pool.end();
            pool = null;
        }
    });

    async function createTestTable() {
        const tableName = `test_tbl_${Math.random()
            .toString()
            .replace(".", "")}`;
        await pool.query(`CREATE TABLE "${tableName}" (
            "id" uuid NOT NULL DEFAULT uuid_generate_v4(),
            "name" varchar(250) NOT NULL DEFAULT ''::character varying,
            "desc" varchar(250) NOT NULL DEFAULT ''::character varying,
            "left" int4 NOT NULL,
            "right" int4 NOT NULL,
            PRIMARY KEY ("id")
        ) WITH (
            OIDS = FALSE
        )`);
        createTables.push(tableName);
        return tableName;
    }

    /**
     * Create test data as followings:
     *
     *         +---------+
     *         |  Albert |
     *         | 1     12|
     *         +----+----+
     *              |
     *     +--------+--------+
     *     |                 |
     * +----+----+       +----+----+
     * |   Bert  |       |  Chuck  |
     * | 2     3 |       | 4     11|
     * +---------+       +----+----+
     *                        |
     *         +-------------------------+
     *         |             |            |
     *     +----+----+  +----+----+  +----+----+
     *     |  Donna  |  |  Eddie  |  |   Fred  |
     *     | 5     6 |  | 7     8 |  | 9     10|
     *     +---------+  +---------+  +---------+
     *
     * @returns
     */
    async function createTestTableWithTestData() {
        const tableName = await createTestTable();
        await pool.query(`INSERT INTO "${tableName}" ("name", "left", "right") VALUES
        ('Albert', 1, 12),
        ('Bert', 2, 3),
        ('Chuck', 4, 11),
        ('Donna', 5, 6),
        ('Eddie', 7, 8),
        ('Fred', 9, 10)`);
        return tableName;
    }

    async function getNodeIdFromName(tableName: string, name: string) {
        const queryer = new NestedSetModelQueryer(pool, tableName);
        const nodes = await queryer.getNodesByName(name);
        if (!nodes) throw new Error(`Can't find node by name: ${name}`);
        return nodes[0]["id"];
    }

    it("Test `getNodesByName`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        const nodes = await queryer.getNodesByName("Chuck");
        expect(nodes.length).to.equal(1);
        expect(nodes[0]["name"]).to.equal("Chuck");
    });

    it("Test `getNodeById`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        const nodes = await queryer.getNodesByName("Chuck");
        expect(nodes.length).to.equal(1);
        expect(nodes[0]["name"]).to.equal("Chuck");
        const testNode = await queryer.getNodeById(nodes[0]["id"]);
        expect(testNode.name).to.equal("Chuck");
    });

    it("Test `getRootNode`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        const node = await queryer.getRootNode();
        expect(node.name).to.equal("Albert");
    });

    it("Test `defaultSelectFieldList` paremeter", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName, [
            "id",
            "left"
        ]);
        const node = await queryer.getRootNode();
        expect(Object.keys(node)).to.have.members(["id", "left"]);
    });

    it("Test `getLeafNodes`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        const nodes = await queryer.getLeafNodes();
        expect(nodes.map(n => n.name)).to.have.members([
            "Bert",
            "Donna",
            "Eddie",
            "Fred"
        ]);
    });

    it("Test `getAllChildren`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let nodes = await queryer.getAllChildren(
            await getNodeIdFromName(tableName, "Albert")
        );
        expect(nodes.map(n => n.name)).to.have.members([
            "Bert",
            "Chuck",
            "Donna",
            "Eddie",
            "Fred"
        ]);

        nodes = await queryer.getAllChildren(
            await getNodeIdFromName(tableName, "Chuck")
        );
        expect(nodes.map(n => n.name)).to.have.members([
            "Donna",
            "Eddie",
            "Fred"
        ]);

        nodes = await queryer.getAllChildren(
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(nodes).be.null;
    });

    it("Test `getImmediateChildren`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let nodes = await queryer.getImmediateChildren(
            await getNodeIdFromName(tableName, "Albert")
        );
        expect(nodes.map(n => n.name)).to.have.members(["Bert", "Chuck"]);

        nodes = await queryer.getImmediateChildren(
            await getNodeIdFromName(tableName, "Chuck")
        );
        expect(nodes.map(n => n.name)).to.have.members([
            "Donna",
            "Eddie",
            "Fred"
        ]);

        nodes = await queryer.getImmediateChildren(
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(nodes).be.null;
    });

    it("Test `getAllParents`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let nodes = await queryer.getAllParents(
            await getNodeIdFromName(tableName, "Chuck")
        );
        expect(nodes.map(n => n.name)).to.have.members(["Albert"]);

        nodes = await queryer.getAllParents(
            await getNodeIdFromName(tableName, "Donna")
        );
        expect(nodes.map(n => n.name)).to.have.members(["Chuck", "Albert"]);

        nodes = await queryer.getAllParents(
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(nodes.map(n => n.name)).to.have.members(["Albert"]);

        nodes = await queryer.getAllParents(
            await getNodeIdFromName(tableName, "Albert")
        );
        expect(nodes).be.null;
    });

    it("Test `getImmediateParent`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let node = await queryer.getImmediateParent(
            await getNodeIdFromName(tableName, "Chuck")
        );
        expect(node.name).to.equal("Albert");

        node = await queryer.getImmediateParent(
            await getNodeIdFromName(tableName, "Donna")
        );
        expect(node.name).to.equal("Chuck");

        node = await queryer.getImmediateParent(
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(node.name).to.equal("Albert");

        node = await queryer.getImmediateParent(
            await getNodeIdFromName(tableName, "Albert")
        );
        expect(node).be.null;
    });

    it("Test `getAllNodesAtLevel`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let nodes = await queryer.getAllNodesAtLevel(1);
        expect(nodes.length).to.equal(1);
        expect(nodes[0].name).to.equal("Albert");

        nodes = await queryer.getAllNodesAtLevel(2);
        expect(nodes.map(n => n.name)).to.have.members(["Bert", "Chuck"]);

        nodes = await queryer.getAllNodesAtLevel(3);
        expect(nodes.map(n => n.name)).to.have.members([
            "Donna",
            "Eddie",
            "Fred"
        ]);

        nodes = await queryer.getAllNodesAtLevel(4);
        expect(nodes).be.null;
    });

    it("Test `getLevelOfNode`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let level: number;
        level = await queryer.getLevelOfNode(
            await getNodeIdFromName(tableName, "Albert")
        );
        expect(level).to.equal(1);

        level = await queryer.getLevelOfNode(
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(level).to.equal(2);

        level = await queryer.getLevelOfNode(
            await getNodeIdFromName(tableName, "Chuck")
        );
        expect(level).to.equal(2);

        level = await queryer.getLevelOfNode(
            await getNodeIdFromName(tableName, "Donna")
        );
        expect(level).to.equal(3);

        level = await queryer.getLevelOfNode(
            await getNodeIdFromName(tableName, "Fred")
        );
        expect(level).to.equal(3);

        level = await queryer.getLevelOfNode(
            await getNodeIdFromName(tableName, "Eddie")
        );
        expect(level).to.equal(3);
    });

    it("Test `getTreeHeight`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let height = await queryer.getTreeHeight();
        expect(height).to.equal(3);
    });

    it("Test `getLeftMostImmediateChild`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let node: NodeRecord;
        node = await queryer.getLeftMostImmediateChild(
            await getNodeIdFromName(tableName, "Albert")
        );
        expect(node.name).to.equal("Bert");

        node = await queryer.getLeftMostImmediateChild(
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(node).be.null;

        node = await queryer.getLeftMostImmediateChild(
            await getNodeIdFromName(tableName, "Chuck")
        );
        expect(node.name).to.equal("Donna");

        node = await queryer.getLeftMostImmediateChild(
            await getNodeIdFromName(tableName, "Eddie")
        );
        expect(node).be.null;
    });

    it("Test `getRightMostImmediateChild`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let node: NodeRecord;
        node = await queryer.getRightMostImmediateChild(
            await getNodeIdFromName(tableName, "Albert")
        );
        expect(node.name).to.equal("Chuck");

        node = await queryer.getRightMostImmediateChild(
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(node).be.null;

        node = await queryer.getRightMostImmediateChild(
            await getNodeIdFromName(tableName, "Chuck")
        );
        expect(node.name).to.equal("Fred");

        node = await queryer.getRightMostImmediateChild(
            await getNodeIdFromName(tableName, "Eddie")
        );
        expect(node).be.null;
    });

    it("Test `getTopDownPathBetween`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let nodes: NodeRecord[];
        nodes = await queryer.getTopDownPathBetween(
            await getNodeIdFromName(tableName, "Albert"),
            await getNodeIdFromName(tableName, "Donna")
        );
        expect(nodes.map(n => n.name)).to.have.members([
            "Albert",
            "Chuck",
            "Donna"
        ]);

        nodes = await queryer.getTopDownPathBetween(
            await getNodeIdFromName(tableName, "Albert"),
            await getNodeIdFromName(tableName, "Eddie")
        );
        expect(nodes.map(n => n.name)).to.have.members([
            "Albert",
            "Chuck",
            "Eddie"
        ]);

        nodes = await queryer.getTopDownPathBetween(
            await getNodeIdFromName(tableName, "Chuck"),
            await getNodeIdFromName(tableName, "Fred")
        );
        expect(nodes.map(n => n.name)).to.have.members(["Chuck", "Fred"]);

        nodes = await queryer.getTopDownPathBetween(
            await getNodeIdFromName(tableName, "Albert"),
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(nodes.map(n => n.name)).to.have.members(["Albert", "Bert"]);

        nodes = await queryer.getTopDownPathBetween(
            await getNodeIdFromName(tableName, "Bert"),
            await getNodeIdFromName(tableName, "Fred")
        );
        // --- there is no path between Bert and Fred
        expect(nodes).be.null;
    });

    it("Test `compareNodes`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        let compareResult: number;
        compareResult = await queryer.compareNodes(
            await getNodeIdFromName(tableName, "Albert"),
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(compareResult).to.equal(1);

        compareResult = await queryer.compareNodes(
            await getNodeIdFromName(tableName, "Bert"),
            await getNodeIdFromName(tableName, "Albert")
        );
        expect(compareResult).to.equal(-1);

        compareResult = await queryer.compareNodes(
            await getNodeIdFromName(tableName, "Bert"),
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(compareResult).to.equal(0);

        compareResult = await queryer.compareNodes(
            await getNodeIdFromName(tableName, "Bert"),
            await getNodeIdFromName(tableName, "Eddie")
        );
        // --- as there is no path between Bert & Eddie
        // --- i.e. Eddie is not Bert's subordinate
        expect(compareResult).be.null;

        compareResult = await queryer.compareNodes(
            await getNodeIdFromName(tableName, "Eddie"),
            await getNodeIdFromName(tableName, "Bert")
        );
        expect(compareResult).be.null;

        compareResult = await queryer.compareNodes(
            await getNodeIdFromName(tableName, "Chuck"),
            await getNodeIdFromName(tableName, "Donna")
        );
        expect(compareResult).to.equal(1);

        compareResult = await queryer.compareNodes(
            await getNodeIdFromName(tableName, "Chuck"),
            await getNodeIdFromName(tableName, "Donna")
        );
        expect(compareResult).to.equal(1);

        compareResult = await queryer.compareNodes(
            await getNodeIdFromName(tableName, "Chuck"),
            "60194a60-aaaa-aaaa-aaaa-3e4d3c2cfefc" //--- non exists node
        );
        expect(compareResult).be.null;
    });

    it("Test `createRootNode`", async () => {
        const tableName = await createTestTable();
        const queryer = new NestedSetModelQueryer(pool, tableName);
        const nodeId = await queryer.createRootNode({
            name: "test root node name",
            desc: "test root node description"
        });
        expect(typeof nodeId).to.equal("string");
        expect(nodeId.length).to.equal(36);

        const result = await pool.query(
            `SELECT * FROM "${tableName}" WHERE "id" = $1`,
            [nodeId]
        );
        expect(_.isArray(result.rows)).to.equal(true);
        expect(result.rows[0].name).to.equal("test root node name");
        expect(result.rows[0].desc).to.equal("test root node description");
        expect(result.rows[0].left).to.equal(1);
        expect(result.rows[0].right).to.equal(2);

        expect(queryer.createRootNode({ name: "abc" })).be.rejectedWith(
            `A root node with id: ${nodeId} already exists`
        );
    });

    it("Test `insertNode`", async () => {
        const tableName = await createTestTable();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        const nodeId = await queryer.createRootNode({
            name: "Albert"
        });
        expect(typeof nodeId).to.equal("string");
        expect(nodeId.length).to.equal(36);

        const rootNode = await queryer.getRootNode();
        await queryer.insertNode({ name: "Bert" }, rootNode["id"]);

        const lv3ParentNodeId = await queryer.insertNode(
            { name: "Chuck" },
            nodeId
        );

        expect(typeof lv3ParentNodeId).to.equal("string");
        expect(lv3ParentNodeId.length).to.equal(36);

        await queryer.insertNode({ name: "Donna" }, lv3ParentNodeId);
        await queryer.insertNode({ name: "Eddie" }, lv3ParentNodeId);
        await queryer.insertNode({ name: "Fred" }, lv3ParentNodeId);

        let testNode = (await queryer.getNodesByName("Albert"))[0];
        expect(testNode.left).to.equal(1);
        expect(testNode.right).to.equal(12);

        testNode = (await queryer.getNodesByName("Bert"))[0];
        expect(testNode.left).to.equal(2);
        expect(testNode.right).to.equal(3);

        testNode = (await queryer.getNodesByName("Chuck"))[0];
        expect(testNode.left).to.equal(4);
        expect(testNode.right).to.equal(11);

        testNode = (await queryer.getNodesByName("Donna"))[0];
        expect(testNode.left).to.equal(5);
        expect(testNode.right).to.equal(6);

        testNode = (await queryer.getNodesByName("Eddie"))[0];
        expect(testNode.left).to.equal(7);
        expect(testNode.right).to.equal(8);

        testNode = (await queryer.getNodesByName("Fred"))[0];
        expect(testNode.left).to.equal(9);
        expect(testNode.right).to.equal(10);
    });

    it("Test `insertNodeToRightOfSibling`", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        const bertId = (await queryer.getNodesByName("Bert"))[0]["id"];
        const bert1nodeId = await queryer.insertNodeToRightOfSibling(
            { name: "Bert1" },
            bertId
        );
        expect(typeof bert1nodeId).to.equal("string");
        expect(bert1nodeId.length).to.equal(36);

        const lv2Nodes = await queryer.getAllNodesAtLevel(2);
        const bert1 = lv2Nodes.find(n => n.id === bert1nodeId);
        expect(_.isUndefined(bert1)).to.equal(false);

        const bert = lv2Nodes.find(n => n.id === bertId);
        expect(_.isUndefined(bert)).to.equal(false);

        const chuckId = (await queryer.getNodesByName("Chuck"))[0]["id"];
        const chuck = lv2Nodes.find(n => n.id === chuckId);
        expect(_.isUndefined(chuck)).to.equal(false);

        // --- test bert1 is between bert and chuck
        expect(bert.left < bert1.left).to.equal(true);
        expect(bert.right < bert1.right).to.equal(true);

        expect(chuck.left > bert1.left).to.equal(true);
        expect(chuck.right > bert1.right).to.equal(true);

        // --- Chuck's childrens should still be his children
        const childrens = await queryer.getAllChildren(chuckId, false);
        expect(childrens.length).to.equal(3);
        expect(childrens.findIndex(n => n.name === "Donna") !== -1).to.equal(
            true
        );
        expect(childrens.findIndex(n => n.name === "Eddie") !== -1).to.equal(
            true
        );
        expect(childrens.findIndex(n => n.name === "Fred") !== -1).to.equal(
            true
        );
    });

    it("Test `moveSubTreeTo` No.1", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        const chuckId = (await queryer.getNodesByName("Chuck"))[0]["id"];
        const bertId = (await queryer.getNodesByName("Bert"))[0]["id"];

        // --- move Chuck under Bert
        await queryer.moveSubTreeTo(chuckId, bertId);

        // --- checking three structure after the chanhes
        let rootNode = await queryer.getRootNode();
        expect(rootNode.name).to.equal("Albert");

        let lv2Nodes = await queryer.getAllNodesAtLevel(2);
        expect(lv2Nodes.length).to.equal(1);
        expect(lv2Nodes[0].name).to.equal("Bert");

        let lv3Nodes = await queryer.getAllNodesAtLevel(3);
        expect(lv3Nodes.length).to.equal(1);
        expect(lv3Nodes[0].name).to.equal("Chuck");

        let lv4Nodes = await queryer.getAllNodesAtLevel(4);
        expect(lv4Nodes.length).to.equal(3);
        expect(lv4Nodes.map(n => n.name)).to.have.members([
            "Donna",
            "Eddie",
            "Fred"
        ]);

        // --- continue to move `Donna` to `Bert`
        await queryer.moveSubTreeTo(
            (await queryer.getNodesByName("Donna"))[0]["id"],
            (await queryer.getNodesByName("Bert"))[0]["id"]
        );

        // --- checking three structure after the chanhes
        rootNode = await queryer.getRootNode();
        expect(rootNode.name).to.equal("Albert");

        lv2Nodes = await queryer.getAllNodesAtLevel(2);
        expect(lv2Nodes.length).to.equal(1);
        expect(lv2Nodes[0].name).to.equal("Bert");

        lv3Nodes = await queryer.getAllNodesAtLevel(3);
        expect(lv3Nodes.length).to.equal(2);
        expect(lv3Nodes.map(n => n.name)).to.have.members(["Chuck", "Donna"]);

        lv4Nodes = await queryer.getAllNodesAtLevel(4);
        expect(lv4Nodes.length).to.equal(2);
        expect(lv4Nodes.map(n => n.name)).to.have.members(["Eddie", "Fred"]);

        // --- we shouldn't allow to move Bert's sub stree to Eddie
        // --- as Eddie is one of Bert's subordinate
        let eddieId = (await queryer.getNodesByName("Eddie"))[0]["id"];

        expect(queryer.moveSubTreeTo(bertId, eddieId)).be.rejectedWith(
            `Cannot move a higher level node (id: ${bertId})to its subordinate (id: ${eddieId})`
        );
    });

    it("Test `moveSubTreeTo` No.2", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        expect(
            (await queryer.getImmediateParent(
                (await queryer.getNodesByName("Donna"))[0]["id"]
            )).name
        ).to.equal("Chuck");

        // --- move `Donna` to `Bert`: there is no path from Donna to Bert
        await queryer.moveSubTreeTo(
            (await queryer.getNodesByName("Donna"))[0]["id"],
            (await queryer.getNodesByName("Bert"))[0]["id"]
        );

        // --- checking three structure after the chanhes
        let rootNode = await queryer.getRootNode();
        expect(rootNode.name).to.equal("Albert");

        let lv2Nodes = await queryer.getAllNodesAtLevel(2);
        expect(lv2Nodes.map(n => n.name)).to.have.members(["Bert", "Chuck"]);

        let lv3Nodes = await queryer.getAllNodesAtLevel(3);
        expect(lv3Nodes.map(n => n.name)).to.have.members([
            "Eddie",
            "Donna",
            "Fred"
        ]);
        expect(
            (await queryer.getImmediateParent(
                (await queryer.getNodesByName("Donna"))[0]["id"]
            )).name
        ).to.equal("Bert");
    });

    it("Test `deleteSubTree` No.1", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        const chuckId = (await queryer.getNodesByName("Chuck"))[0]["id"];
        await queryer.deleteSubTree(chuckId);

        // --- checking left nodes data
        await checkNodeLeftRight(queryer, "Albert", 1, 4);
        await checkNodeLeftRight(queryer, "Bert", 2, 3);

        const results = await pool.query(`SELECT * FROM "${tableName}"`);
        expect(results.rows.length).to.equal(2);
    });

    it("Test `deleteSubTree` No.2 allow delete root node", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        // --- try delete root stree
        const rootNodeId = (await queryer.getNodesByName("Albert"))[0]["id"];
        await queryer.deleteSubTree(rootNodeId, true);

        const results = await pool.query(`SELECT * FROM "${tableName}"`);
        expect(results.rows.length).to.equal(0);
    });

    it("Test `deleteSubTree` No.3 not allow delete root node", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        // --- try delete root stree
        const rootNodeId = (await queryer.getNodesByName("Albert"))[0]["id"];

        expect(queryer.deleteSubTree(rootNodeId)).be.rejectedWith(
            "Root node id is not allowed!"
        );
    });

    it("Test `deleteSubTree` no.4 delete a leaf node", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        // --- move Bert under Eddie
        await queryer.moveSubTreeTo(
            (await queryer.getNodesByName("Donna"))[0]["id"],
            (await queryer.getNodesByName("Bert"))[0]["id"]
        );

        // --- checking left nodes data
        await checkNodeLeftRight(queryer, "Albert", 1, 12);
        await checkNodeLeftRight(queryer, "Bert", 2, 5);
        await checkNodeLeftRight(queryer, "Chuck", 6, 11);
        await checkNodeLeftRight(queryer, "Donna", 3, 4);
        await checkNodeLeftRight(queryer, "Eddie", 7, 8);
        await checkNodeLeftRight(queryer, "Fred", 9, 10);

        // --- delete Eddie
        await queryer.deleteSubTree(
            (await queryer.getNodesByName("Eddie"))[0]["id"]
        );

        // --- checking left nodes data
        await checkNodeLeftRight(queryer, "Albert", 1, 10);
        await checkNodeLeftRight(queryer, "Bert", 2, 5);
        await checkNodeLeftRight(queryer, "Chuck", 6, 9);
        await checkNodeLeftRight(queryer, "Donna", 3, 4);
        await checkNodeLeftRight(queryer, "Fred", 7, 8);
    });

    it("Test `deleteNode` no. 1", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        const chuckId = (await queryer.getNodesByName("Chuck"))[0]["id"];

        // --- delete chuck
        await queryer.deleteNode(chuckId);

        // --- checking left nodes data
        await checkNodeLeftRight(queryer, "Albert", 1, 10);
        await checkNodeLeftRight(queryer, "Bert", 2, 3);
        await checkNodeLeftRight(queryer, "Donna", 4, 5);
        await checkNodeLeftRight(queryer, "Eddie", 6, 7);
        await checkNodeLeftRight(queryer, "Fred", 8, 9);
    });

    it("Test `deleteNode` no.2", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        // --- move Bert under Eddie
        await queryer.moveSubTreeTo(
            (await queryer.getNodesByName("Bert"))[0]["id"],
            (await queryer.getNodesByName("Eddie"))[0]["id"]
        );

        // --- move Donna under Bert
        await queryer.moveSubTreeTo(
            (await queryer.getNodesByName("Donna"))[0]["id"],
            (await queryer.getNodesByName("Bert"))[0]["id"]
        );

        // --- checking left nodes data
        await checkNodeLeftRight(queryer, "Albert", 1, 12);
        await checkNodeLeftRight(queryer, "Chuck", 2, 11);
        await checkNodeLeftRight(queryer, "Eddie", 3, 8);
        await checkNodeLeftRight(queryer, "Bert", 4, 7);
        await checkNodeLeftRight(queryer, "Donna", 5, 6);
        await checkNodeLeftRight(queryer, "Fred", 9, 10);

        // --- delete Eddie
        await queryer.deleteNode(
            (await queryer.getNodesByName("Eddie"))[0]["id"]
        );

        // --- checking left nodes data
        await checkNodeLeftRight(queryer, "Albert", 1, 10);
        await checkNodeLeftRight(queryer, "Chuck", 2, 9);
        await checkNodeLeftRight(queryer, "Bert", 3, 6);
        await checkNodeLeftRight(queryer, "Donna", 4, 5);
        await checkNodeLeftRight(queryer, "Fred", 7, 8);
    });

    it("Test `deleteNode` no.3", async () => {
        const tableName = await createTestTableWithTestData();
        const queryer = new NestedSetModelQueryer(pool, tableName);

        queryer.defaultSelectFieldList = ["id", "name", "left", "right"];

        // --- move Bert under Eddie
        await queryer.moveSubTreeTo(
            (await queryer.getNodesByName("Bert"))[0]["id"],
            (await queryer.getNodesByName("Eddie"))[0]["id"]
        );

        // --- move Donna under Bert
        await queryer.moveSubTreeTo(
            (await queryer.getNodesByName("Donna"))[0]["id"],
            (await queryer.getNodesByName("Bert"))[0]["id"]
        );

        // --- checking left nodes data
        await checkNodeLeftRight(queryer, "Albert", 1, 12);
        await checkNodeLeftRight(queryer, "Chuck", 2, 11);
        await checkNodeLeftRight(queryer, "Eddie", 3, 8);
        await checkNodeLeftRight(queryer, "Bert", 4, 7);
        await checkNodeLeftRight(queryer, "Donna", 5, 6);
        await checkNodeLeftRight(queryer, "Fred", 9, 10);

        // --- delete Eddie
        await queryer.deleteNode(
            (await queryer.getNodesByName("Bert"))[0]["id"]
        );

        // --- checking left nodes data
        await checkNodeLeftRight(queryer, "Albert", 1, 10);
        await checkNodeLeftRight(queryer, "Chuck", 2, 9);
        await checkNodeLeftRight(queryer, "Eddie", 3, 6);
        await checkNodeLeftRight(queryer, "Donna", 4, 5);
        await checkNodeLeftRight(queryer, "Fred", 7, 8);
    });
});
