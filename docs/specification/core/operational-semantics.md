# Operational syntax

TODO

## Evaluation

| `eval(`<var>globals</var>`,` <var>offset</var>`,` <var>term</var>`)` | <var>value</var> |
| - | - |
| `eval(`<var>globals</var>`,` <var>offset</var>`,` `univ(`<var>level</var>`))` | `univ(`<var>level</var> + <var>offset</var>`)` |
| `eval(`<var>globals</var>`,` <var>offset</var>`,` `local(`<var>name</var>`))` | `local(`<var>name</var>`)` |
| `eval(`<var>globals</var>`,` <var>offset</var>`,` `global(`<var>name</var>`))` | `global(`<var>name</var>`)` |
| `eval(`<var>globals</var>`,` <var>offset</var>`,` `constant(`<var>constant</var>`))` | `constant(`constant`)` |
| `eval(`<var>globals</var>`,` <var>offset</var>`,` `ann(`<var>term</var>`,` <var>type</var>`))` | `eval(`<var>globals</var>`,` <var>offset</var>`,` <var>term</var>`)` |
| &hellip; | &hellip; |
| `eval(`<var>globals</var>`,` <var>offset</var><sub>0</sub>`,` `lift(`<var>term</var>`,` <var>offset</var><sub>1</sub>`))` | `eval(`<var>globals</var>`,` <var>offset</var><sub>0</sub> + <var>offset</var><sub>1</sub>`,` <var>term</var>`)` |

## Read-back

| `read-back(`<var>value</var>`,`<var>value</var>`)` | <var>term</var> |
| - | - |
| &hellip; | &hellip; |
