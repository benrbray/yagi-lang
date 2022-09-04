"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.rangeHover = void 0;
const vscode_languageclient_1 = require("vscode-languageclient");
// adapted from
// - [rust-analyzer](https://github.com/rust-lang/rust-analyzer/pull/9693/files#)
// - [metals-vscode](https://github.com/scalameta/metals-vscode/blob/66c2bcc36fbbeb9a1416e51cf29df7284fa99d0d/src/hoverExtension.ts)
exports.rangeHover = new vscode_languageclient_1.RequestType("textDocument/hover");
//# sourceMappingURL=rangeHover.js.map