//// [renderer.d.ts]
export { dom as default };
//// [otherreacty.tsx]
/** @jsx React.createElement */ import * as React from "./renderer";
/*#__PURE__*/ React.createElement("h", null);
//// [other.tsx]
/** @jsx h */ import { dom as h } from "./renderer";
export var prerendered = /*#__PURE__*/ h("h", null);
//// [othernoalias.tsx]
/** @jsx otherdom */ import { otherdom } from "./renderer";
export var prerendered2 = /*#__PURE__*/ otherdom("h", null);
//// [reacty.tsx]
import React from "./renderer";
export var prerendered3 = /*#__PURE__*/ React.createElement("h", null);
//// [index.tsx]
/** @jsx dom */ import { dom } from "./renderer";
/*#__PURE__*/ dom("h", null);
export * from "./other";
export * from "./othernoalias";
export * from "./reacty";
