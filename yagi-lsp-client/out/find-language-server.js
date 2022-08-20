"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.executableExists = exports.findLanguageServer = void 0;
const child_process = require("child_process");
const which = require("which");
const utils_1 = require("./utils");
const error_1 = require("./error");
////////////////////////////////////////////////////////////////////////////////
async function findLanguageServer(logger, yagiConfig, folder) {
    logger.info(`attempting to find the server executable in: ${yagiConfig.serverExecutablePath}`);
    const expandedPath = utils_1.expandPathVariables(yagiConfig.serverExecutablePath, folder);
    logger.log(`location after path variables substitution: ${expandedPath}`);
    if (executableExists(expandedPath)) {
        return expandedPath;
    }
    else {
        throw new error_1.LanguageServerNotFoundError(expandedPath);
    }
}
exports.findLanguageServer = findLanguageServer;
function executableExists(exe) {
    const isWindows = process.platform === 'win32';
    let newEnv = {};
    newEnv = { ...process.env, ...newEnv };
    const cmd = isWindows ? 'where' : 'which';
    const out = child_process.spawnSync(cmd, [exe], { env: newEnv });
    return out.status === 0 || (which.sync(exe, { nothrow: true, path: newEnv.PATH }) ?? '') !== '';
}
exports.executableExists = executableExists;
//# sourceMappingURL=find-language-server.js.map