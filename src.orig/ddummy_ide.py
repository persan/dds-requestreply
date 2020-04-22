
import GPS

with open("dummy_ide.py", "w") as o:
    for i in a:
        if "/adainclude/" not in i:
            o.write('GPS.Action("%s").disable(disabled=True)\n' % i)
