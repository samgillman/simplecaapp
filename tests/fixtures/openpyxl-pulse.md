# Excel regression fixture

`openpyxl-pulse.xlsx` was generated with openpyxl 3.1.5 (no external/user data).
It has one worksheet, headers `Time` and `Cell1`, and 60 samples:

- Time: 0–59 seconds.
- Cell1: 100 for samples 1–20, 150 for 21–30, 100 for 31–60.
- With baseline frames 1–20: F0 = 100, peak ΔF/F0 = 0.5.

Generation:

```python
from openpyxl import Workbook
w = Workbook()
s = w.active
s.append(["Time", "Cell1"])
for t in range(60):
    s.append([t, 150 if 20 <= t < 30 else 100])
w.save("openpyxl-pulse.xlsx")
```
