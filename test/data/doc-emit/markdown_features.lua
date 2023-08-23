--[[- This is a simple module which contains a function

# References:
No label: @{some_value} [`some_value`]

With label @{some_value|label} [label][`some_value`]

External modules: @{package.path|module path} [module path][`package.path`]

Across multiple lines: @{some_value|a multiline
label}. [a multiline
label][`some_value`]

# Admonitions
## Old syntax
:::warning
Some warning
:::

:::warning With a label
Some warning
:::

## New syntax
> [!WARNING]
> Critical content demanding immediate user attention due to potential risks.

> [With a label][!WARNING]
> Critical content demanding immediate user attention due to potential risks.

# Colours
#fff #fda087

# Attributes on code blocks

```lua {#some-id data-x=x}
print("Hello")
```

```python {#some-id data-x=x}
print("Hello")
```

# Attributes on headers
## Some header    {#foo}

@module x
]]

return { some_value = 0 }
