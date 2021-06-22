# SPDX-License-Identifier: Apache-2.0

cmake_minimum_required(VERSION 3.13.1)
find_package(Zephyr REQUIRED HINTS $ENV{ZEPHYR_BASE})
project(blinky-ssm-test)

set(LLBC ../../Sense-VM/backend/src/ll)
set(LLBC_Z ../../Sense-VM/backend/src/ll/os/zephyr)

set(LLBH ../../Sense-VM/backend/include)
set(LLBH_Z ../../Sense-VM/backend/include/ll/os/zephyr)

set(SSM ../runtime)
set(SSMRTS ../runtime/include)
set(SSMPLATFORM ../runtime/linux/include)

file(GLOB app_sources src/*.c)
file(GLOB extra_sources
  ${LLBC}/ll_driver.c
  ${LLBC}/ll_led.c
  ${LLBC}/ll_button.c
  ${LLBC}/os/zephyr/led.c
  ${LLBC}/os/zephyr/button.c
  ${SSM}/src/*.c
)

target_sources(app PRIVATE ${app_sources} ${extra_sources})
target_include_directories(app PRIVATE
  ${LLBH}
  ${LLBH_Z}
  ${ZEPHYR_BASE}/kernel/include
  ${ZEPHYR_BASE}/arch/${ARCH}/include
  ${SSMRTS}
  ${SSMPLATFORM}
  include/
  )
