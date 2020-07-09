--- This is a simple module which contains a function
-- @module fancy.module

local M = {}

function M.a() end --- A module method
M.b = M.a

local T = {} --- @type T

function T.a() end --- A type method
T.b = M.a --- @see T.a

return M
