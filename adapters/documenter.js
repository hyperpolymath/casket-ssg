// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Documenter.jl adapter - Documentation generator for Julia
 * https://documenter.juliadocs.org/
 */

export const name = "Documenter.jl";
export const language = "Julia";
export const description = "Documentation generator for Julia packages";

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
    name: "documenter_makedocs",
    description: "Build documentation",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        sitename: { type: "string", description: "Site name" },
      },
    },
    execute: async ({ path, sitename }) => {
      const sn = sitename ? `sitename="${sitename}"` : "";
      return await runJulia(`using Documenter; makedocs(${sn})`, path);
    },
  },
  {
    name: "documenter_deploydocs",
    description: "Deploy documentation",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        repo: { type: "string", description: "Repository URL" },
      },
    },
    execute: async ({ path, repo }) => {
      const r = repo ? `repo="${repo}"` : "";
      return await runJulia(`using Documenter; deploydocs(${r})`, path);
    },
  },
  {
    name: "documenter_doctest",
    description: "Run doctests",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        module: { type: "string", description: "Module name" },
      },
      required: ["module"],
    },
    execute: async ({ path, module }) => {
      return await runJulia(`using Documenter, ${module}; doctest(${module})`, path);
    },
  },
  {
    name: "documenter_serve",
    description: "Serve documentation locally with LiveServer",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to docs/build" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const p = port || 8000;
      return await runJulia(`using LiveServer; serve(dir="docs/build", port=${p})`, path);
    },
  },
  {
    name: "documenter_version",
    description: "Get Julia/Documenter version",
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
