import GPS
lines = 0
for f in GPS.Project.root().sources(True):
    with open(f.name()) as inf:
        lines = lines + len(inf.read().split("\n"))
print(lines)
