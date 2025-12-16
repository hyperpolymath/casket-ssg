// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Cryogen adapter - Static site generator in Clojure
 * https://cryogenweb.org/
 */

export const name = "Cryogen";
export const language = "Clojure";
export const description = "Simple static site generator written in Clojure";

let connected = false;
let leinPath = "lein";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(leinPath, {
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
    const result = await runCommand(["version"]);
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
    name: "cryogen_new",
    description: "Create a new Cryogen site",
    inputSchema: {
      type: "object",
      properties: {
        name: { type: "string", description: "Project name" },
        path: { type: "string", description: "Path for the project" },
      },
      required: ["name"],
    },
    execute: async ({ name, path }) => {
      return await runCommand(["new", "cryogen", name], path);
    },
  },
  {
    name: "cryogen_build",
    description: "Build the Cryogen site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["run"], path),
  },
  {
    name: "cryogen_serve",
    description: "Start Cryogen server with live reload",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["ring", "server"], path),
  },
  {
    name: "cryogen_clean",
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
    name: "cryogen_deps",
    description: "Fetch dependencies",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runCommand(["deps"], path),
  },
  {
    name: "cryogen_version",
    description: "Get Leiningen version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["version"]),
  },
];
