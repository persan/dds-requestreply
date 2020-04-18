# This file contains  GNATStudio customisations.
import GPS
try:
    from gs_utils import hook
except:
    from gps_utils import hook

GPS.Action("Build & Run number 1").key("control-F4", True)

