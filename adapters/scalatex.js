// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * ScalaTex adapter - Programmable documentation in Scala
 * https://www.lihaoyi.com/Scalatex/
 */

export const name = "ScalaTex";
export const language = "Scala";
export const description = "Programmable, typesafe document generation in Scala";

let connected = false;
let millPath = "mill";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(millPath, {
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

async function runSbt(args, cwd = null) {
  const cmd = new Deno.Command("sbt", {
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
    if (!connected) {
      const sbtResult = await runSbt(["--version"]);
      connected = sbtResult.success;
    }
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
    name: "scalatex_compile",
    description: "Compile ScalaTex documents",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        module: { type: "string", description: "Module to compile" },
      },
    },
    execute: async ({ path, module }) => {
      const mod = module || "docs";
      return await runCommand([`${mod}.compile`], path);
    },
  },
  {
    name: "scalatex_run",
    description: "Run ScalaTex generation",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        module: { type: "string", description: "Module to run" },
      },
    },
    execute: async ({ path, module }) => {
      const mod = module || "docs";
      return await runCommand([`${mod}.run`], path);
    },
  },
  {
    name: "scalatex_watch",
    description: "Watch and rebuild on changes",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        module: { type: "string", description: "Module to watch" },
      },
    },
    execute: async ({ path, module }) => {
      const mod = module || "docs";
      return await runCommand(["--watch", `${mod}.compile`], path);
    },
  },
  {
    name: "scalatex_clean",
    description: "Clean build output",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["clean"], path),
  },
  {
    name: "scalatex_version",
    description: "Get Mill version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
