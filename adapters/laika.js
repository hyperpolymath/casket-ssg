// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Jonathan D.A. Jewell

/**
 * Laika adapter - Site and documentation generator in Scala
 * https://typelevel.org/Laika/
 */

export const name = "Laika";
export const language = "Scala";
export const description = "Customizable site and e-book generator in Scala";

let connected = false;
let sbtPath = "sbt";

async function runCommand(args, cwd = null) {
  const cmd = new Deno.Command(sbtPath, {
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
    name: "laika_site",
    description: "Generate HTML site",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["laikaSite"], path),
  },
  {
    name: "laika_pdf",
    description: "Generate PDF document",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["laikaPDF"], path),
  },
  {
    name: "laika_epub",
    description: "Generate EPUB e-book",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["laikaEPUB"], path),
  },
  {
    name: "laika_preview",
    description: "Start preview server",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
        port: { type: "number", description: "Port number" },
      },
    },
    execute: async ({ path, port }) => {
      const args = ["laikaPreview"];
      // Port config via build.sbt
      return await runCommand(args, path);
    },
  },
  {
    name: "laika_clean",
    description: "Clean generated output",
    inputSchema: {
      type: "object",
      properties: {
        path: { type: "string", description: "Path to project root" },
      },
    },
    execute: async ({ path }) => await runCommand(["clean"], path),
  },
  {
    name: "laika_version",
    description: "Get sbt version",
    inputSchema: { type: "object", properties: {} },
    execute: async () => await runCommand(["--version"]),
  },
];
