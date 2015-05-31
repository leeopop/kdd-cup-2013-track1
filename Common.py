import json
import os

def get_paths():
	paths = json.loads(open("SETTINGS.json").read())
	for key in paths:
		paths[key] = os.path.expandvars(paths[key])
	return paths
