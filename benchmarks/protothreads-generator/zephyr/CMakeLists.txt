cmake_minimum_required(VERSION 3.13.1)
include($ENV{ZEPHYR_BASE}/cmake/app/boilerplate.cmake NO_POLICY_SCOPE)
project(my_zephyr_app)

# Add your source file to the "app" target. This must come after the boilerplate
# code, which defines the target. Note relative path to source file
target_sources(app PRIVATE ../src/main.c)
target_include_directories(app PRIVATE ../include)
