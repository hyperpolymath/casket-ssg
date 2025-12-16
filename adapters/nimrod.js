// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Nimrod adapter - Static site generator in Nim
 * (Nim SSG tools like nimib, nimibook, etc.)
 */

export const name = "Nimrod";
export const language = "Nim";
export const description = "Static site generation using Nim (nimib, nimibook ecosystem)";

let connected = false;
let nimblePath = "nimble";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(nimblePath, {
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

async function runNim(args, cwd = null) {
  const cmd = new Deno.Command("nim", {
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
    name: "nimrod_init",
    description: "Initialize a new Nim project with nimib",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new project" },
        name: { type: "string", description: "Project name" },
      },
      required: ["name"],
    },
    execute: async ({ path, name }) => await runCommand(["init", name], path),
  },
  {
    name: "nimrod_build",
    description: "Build Nim project",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        release: { type: "boolean", description: "Build in release mode" },
      },
    },
    execute: async ({ path, release }) => {
      const args = ["build"];
      if (release) args.push("--release");
      return await runCommand(args, path);
    },
  },
  {
    name: "nimrod_run",
    description: "Run site generator script",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        script: { type: "string", description: "Script to run" },
      },
    },
    execute: async ({ path, script }) => {
      const args = ["c", "-r"];
      if (script) args.push(script);
      return await runNim(args, path);
    },
  },
  {
    name: "nimrod_install",
    description: "Install nimib/nimibook packages",
    inputSchema: {
      type: "object",
      properties: {
        package: { type: "string", description: "Package name (nimib, nimibook, etc.)" },
      },
    },
    execute: async ({ package: pkg }) => {
      return await runCommand(["install", pkg || "nimib"], null);
    },
  },
  {
    name: "nimrod_deps",
    description: "Install project dependencies",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["install", "-d"], path),
  },
  {
    name: "nimrod_docs",
    description: "Generate documentation",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        file: { type: "string", description: "Source file" },
      },
    },
    execute: async ({ path, file }) => {
      const args = ["doc"];
      if (file) args.push(file);
      return await runNim(args, path);
    },
  },
  {
    name: "nimrod_version",
    description: "Get Nim/Nimble version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
