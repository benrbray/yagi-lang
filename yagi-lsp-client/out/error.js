"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.LanguageServerNotFoundError = void 0;
class LanguageServerNotFoundError extends Error {
    constructor(path) {
        super(`Could not find a language server binary at ${path}!  Set a new \`serverExecutablePath\` in the extension settings.`);
        this.path = path;
    }
}
exports.LanguageServerNotFoundError = LanguageServerNotFoundError;
//# sourceMappingURL=error.js.map