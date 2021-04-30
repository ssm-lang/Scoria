
all:
	west build -p auto -b nrf52840dk_nrf52840 .

dev_kit:
	west build -p auto -b nrf52840dk_nrf52840 .

# The nrf_mod board files are not included in the repo
# nrf_mod:
# 	west build -p auto -b nrf_module_v1 -- -DBOARD_ROOT=../ .

clean:
	rm -r build/
