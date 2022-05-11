import os
Import("env")

env.Replace(COMPILATIONDV_PATH=os.path.join("$PROJECT_DIR", "compile_commands.json"))
