// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Ema adapter - Static site generator in Haskell with hot reload
 * https://ema.srid.ca/
 */

export const name = "Ema";
export const language = "Haskell";
export const description = "Next-gen Haskell static site generator with hot reload and Nix support";

let connected = false;
let emaPath = "ema";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(emaPath, {
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

async function runNixCommand(args, cwd = null) {
  const cmd = new Deno.Command("nix", {
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
    // Ema is typically run via nix
    const result = await runNixCommand(["--version"]);
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
    name: "ema_init",
    description: "Initialize a new Ema site from template",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
        template: { type: "string", description: "Template to use (default: ema-template)" },
      },
      required: ["path"],
    },
    execute: async ({ path, template }) => {
      const tmpl = template || "srid/ema-template";
      return await runNixCommand(["flake", "init", "-t", `github:${tmpl}`], path);
    },
  },
  {
    name: "ema_run",
    description: "Run Ema development server with hot reload",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runNixCommand(["run"], path),
  },
  {
    name: "ema_build",
    description: "Build the Ema site for production",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runNixCommand(["build"], path),
  },
  {
    name: "ema_develop",
    description: "Enter development shell",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runNixCommand(["develop"], path),
  },
  {
    name: "ema_update",
    description: "Update flake inputs",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => await runNixCommand(["flake", "update"], path),
  },
  {
    name: "ema_version",
    description: "Get Nix version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runNixCommand(["--version"]),
  },
];
