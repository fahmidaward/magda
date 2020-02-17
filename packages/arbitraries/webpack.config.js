const path = require("path");
const configFile = path.resolve(__dirname, "./tsconfig.json");

module.exports = {
    entry: "./src/index.ts",
    mode: "production",
    target: "node",
    module: {
        rules: [
            {
                test: /\.tsx?$/,
                exclude: /node_modules/,
                loader: "ts-loader",
                options: {
                    configFile
                }
            }
        ]
    },
    stats: "errors-only",
    optimization: {
        minimize: true
    },
    resolve: {
        extensions: [".tsx", ".ts", ".js", ".json"]
    },
    output: {
        libraryTarget: "umd",
        filename: "index.js",
        path: path.resolve(__dirname, "./dist")
    }
};
