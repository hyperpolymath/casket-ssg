// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * YOCaml adapter - Static site generator in OCaml
 * https://github.com/xhtmlboi/yocaml
 */

export const name = "YOCaml";
export const language = "OCaml";
export const description = "Composable static site generator written in OCaml";

let connected = false;
let dunePath = "dune";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(dunePath, {
    args,
    cwd: cwd || Deno.cwd(),
    stdout: "piped",
    stderr: "piped",
  });
  const output = await cmd.output();
  const decoder = new TextDecoder();
  return {
    success: output.success,
    stdout: decoder.decode(output.stdout),
    stderr: decoder.decode(output.stderr),
    code: output.code,
  };
}

async function runOpam(args, cwd = null) {
  const cmd = new Deno.Command("opam", {
    args,
    cwd: cwd || Deno.cwd(),
    stdout: "piped",
    stderr: "piped",
  });
  const output = await cmd.output();
  const decoder = new TextDecoder();
  return {
    success: output.success,
    stdout: decoder.decode(output.stdout),
    stderr: decoder.decode(output.stderr),
    code: output.code,
  };
}

export async function connect() {
  try {
    const result = await runCommand(["--version"]);
    connected = result.success;
    return connected;
  } catch {
    connected = false;
    return false;
  }
}

export async function disconnect() {
  connected = false;
}

export function isConnected() {
  return connected;
}

export const tools = [
  {
    name: "yocaml_init",
    description: "Initialize a new YOCaml project",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
        name: { type: "string", description: "Project name" },
      },
      required: ["name"],
    },
    execute: async ({ path, name }) => {
      return await runCommand(["init", "project", name], path);
    },
  },
  {
    name: "yocaml_build",
    description: "Build the YOCaml site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["build"], path),
  },
  {
    name: "yocaml_exec",
    description: "Execute the site generator",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        target: { type: "string", description: "Build target" },
      },
    },
    execute: async ({ path, target }) => {
      const args = ["exec", target || "./bin/main.exe"];
      return await runCommand(args, path);
    },
  },
  {
    name: "yocaml_clean",
    description: "Clean build artifacts",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["clean"], path),
  },
  {
    name: "yocaml_deps",
    description: "Install dependencies via opam",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runOpam(["install", ".", "--deps-only"], path),
  },
  {
    name: "yocaml_version",
    description: "Get Dune version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
