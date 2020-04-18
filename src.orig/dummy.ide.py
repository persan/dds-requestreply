import GPS
from gs_utils import hook

@hook("file_edited")
def on_file_edited(file):
    """prevent files from being edited by accedent."""
    GPS.EditorBuffer.get(file).set_read_only(True)
