---
title: DAX Feature Test Page
author: Test Suite
date: 2026-01-24
---

# (:title | capitalize)

**Author:** (:author)
**Project:** (:name)
**Version:** (:version)

## Download Count Test (Filter)

Downloads: **(:downloads | thousands-separator)**

## Loop Iteration Test

Tags:
{{#for tag in tags}}
- (:tag | uppercase)
{{/for}}

## Conditional Test

{{#if phase == beta}}
⚠️ This project is in **beta** phase.
{{/if}}

## All Features Working!

This page tests:
1. ✅ Frontmatter parsing (title, author, date)
2. ✅ Filter syntax (| capitalize, | thousands-separator, | uppercase)
3. ✅ Loop iteration ({{#for}})
4. ✅ Conditional rendering ({{#if}})
