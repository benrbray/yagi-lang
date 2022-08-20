"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.expandPathVariables = void 0;
const os = require("os");
function expandPathVariables(path, folder) {
    path = path
        .replace('${HOME}', os.homedir)
        .replace('${home}', os.homedir)
        .replace(/^~/, os.homedir);
    if (!folder) {
        return path;
    }
    path = path
        .replace('${workspaceRoot}', folder.uri.path);
    return path;
}
exports.expandPathVariables = expandPathVariables;
//# sourceMappingURL=utils.js.map