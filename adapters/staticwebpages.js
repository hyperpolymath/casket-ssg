// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * StaticWebPages.jl adapter - Academic website generator in Julia
 * https://github.com/Azzaare/StaticWebPages.jl
 */

export const name = "StaticWebPages.jl";
export const language = "Julia";
export const description = "Academic and personal website generator in Julia";

let connected = false;
let juliaPath = "julia";

async function runJulia(code, cwd = null) {
  const cmd = new Deno.Command(juliaPath, {
    args: ["-e", code],
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
    const cmd = new Deno.Command(juliaPath, {
      args: ["--version"],
      stdout: "piped",
      stderr: "piped",
    });
    const output = await cmd.output();
    connected = output.success;
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
    name: "staticwebpages_init",
    description: "Initialize a new StaticWebPages site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path for the new site" },
      },
    },
    execute: async ({ path }) => {
      return await runJulia(`using StaticWebPages; init("${path || "."}")`, path);
    },
  },
  {
    name: "staticwebpages_build",
    description: "Build the site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
      },
    },
    execute: async ({ path }) => {
      return await runJulia(`using StaticWebPages; build()`, path);
    },
  },
  {
    name: "staticwebpages_serve",
    description: "Serve the site locally",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to site root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const p = port || 8000;
      return await runJulia(`using StaticWebPages; serve(port=${p})`, path);
    },
  },
  {
    name: "staticwebpages_version",
    description: "Get Julia version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => {
      const cmd = new Deno.Command(juliaPath, {
        args: ["--version"],
        stdout: "piped",
        stderr: "piped",
      });
      const output = await cmd.output();
      const decoder = new TextDecoder();
      return {
        success: output.success,
        stdout: decoder.decode(output.stdout),
        stderr: decoder.decode(output.stderr),
      };
    },
  },
];
