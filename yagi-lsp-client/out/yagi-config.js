"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.getYagiExtensionConfig = void 0;
const vscode_1 = require("vscode");
const logger_1 = require("./logger");
////////////////////////////////////////////////////////////
function getYagiExtensionConfig(uri) {
    const config = vscode_1.workspace.getConfiguration("yagi", uri);
    return {
        logLevel: {
            server: config.get("logLevel.server", "off"),
            client: clientLogLevel(config.get("logLevel.server", "off"))
        },
        logFile: config.get("logFile", ""),
        serverExecutablePath: config.get("serverExecutablePath", ""),
        serverExtraArgs: config.get("serverExtraArgs", "")
    };
}
exports.getYagiExtensionConfig = getYagiExtensionConfig;
////////////////////////////////////////////////////////////
function clientLogLevel(level) {
    switch (level) {
        case "off": return logger_1.LogLevel.Off;
        case "error": return logger_1.LogLevel.Error;
        case "warn": return logger_1.LogLevel.Warn;
        case "info": return logger_1.LogLevel.Info;
        case "debug": return logger_1.LogLevel.Debug;
        default: return logger_1.LogLevel.Info;
    }
}
//# sourceMappingURL=yagi-config.js.map