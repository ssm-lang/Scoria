
// freqCount :: Ref SW -> Ref Time -> SSM ()
// freqCount sw period = routine $ do
//     gate  <- var event
//     count <- var $ u64 0

//     after (secs 1) gate event
//     while true $ do
//         if changed gate
//             then do
//                 period <~ secs 1 /. nsecs (deref count)
//                 -- print frequency
//                 if changed sw
//                     then count <~ 1
//                     else count <~ 0
//                 after (secs 1) gate event
//                 wait gate -- sleep for 1 sec
//                 after (secs 1) gate event
//             else count <~ deref count + 1
//         wait (gate, sw)

#include <zephyr.h>
#include <device.h>
#include <drivers/gpio.h>
#include <sys/util.h>
#include <sys/printk.h>
#include <inttypes.h>

#include <drivers/counter.h>

#include <pt.h>

#define SW0_NODE DT_ALIAS(sw0)
#if !DT_NODE_HAS_STATUS(SW0_NODE, okay)
#error "Unsupported board: sw0 devicetree alias is not defined"
#endif

#define BUTTON4_NODE DT_ALIAS(ssm_button)
#if !DT_NODE_HAS_STATUS(BUTTON4_NODE, okay)
#error "button4 device alias not defined"
#endif

static const struct gpio_dt_spec button = GPIO_DT_SPEC_GET_OR(BUTTON4_NODE, gpios,
															  {0});

static struct gpio_dt_spec led = GPIO_DT_SPEC_GET_OR(DT_ALIAS(led0), gpios,
													 {0});

static struct gpio_callback button_cb_data;

#if !DT_NODE_HAS_STATUS(DT_ALIAS(ssm_timer), okay)
#error "ssm-timer device is not supported on this board"
#endif

static void counter_interrupt_fn(const struct device *counter_dev,
								 uint8_t chan_id, uint32_t ticks,
								 void *user_data);

const struct device *timer_dev;
struct counter_alarm_cfg alarm_cfg;


void set_alarm(uint64_t delay_in_us)
{
	alarm_cfg.ticks = counter_us_to_ticks(timer_dev, delay_in_us);
	counter_set_channel_alarm(timer_dev, 0,
							  &alarm_cfg);
}

// The program ******

struct pt generator_pt;
struct pt button_pt;

uint32_t half_period;
uint8_t next;

static PT_THREAD(generator_thread(struct pt *pt))
{
	PT_BEGIN(pt);
	gpio_pin_set_dt(&led, next = next ? 0 : 1);
	set_alarm(half_period);
	PT_YIELD(pt);
	PT_RESTART(pt);
	PT_END(pt);
}

static PT_THREAD(button_thread(struct pt *pt))
{
	PT_BEGIN(pt);
	// idk, let's decrease half period in some way
	if(half_period > 100000) {
		half_period = half_period / 2;
	} else if(half_period > 10000) {
		half_period = half_period - 1000;
	} else {
		half_period = half_period - 100;
	}
	printk("half_period: %d\n", half_period);
	PT_YIELD(pt);
    PT_RESTART(pt);
	PT_END(pt);
}

// End of program ******

void button_pressed(const struct device *dev, struct gpio_callback *cb,
					uint32_t pins)
{
	button_thread(&button_pt);
}

static void counter_interrupt_fn(const struct device *counter_dev,
								 uint8_t chan_id, uint32_t ticks,
								 void *user_data)
{
	generator_thread(&generator_pt);
}

void main()
{
	int ret;

	if (!device_is_ready(button.port))
	{
		printk("Error: button device %s is not ready\n",
			   button.port->name);
		return;
	}

	ret = gpio_pin_configure_dt(&button, GPIO_INPUT);
	if (ret != 0)
	{
		printk("Error %d: failed to configure %s pin %d\n",
			   ret, button.port->name, button.pin);
		return;
	}

	ret = gpio_pin_interrupt_configure_dt(&button,
										  GPIO_INT_EDGE_BOTH);
	if (ret != 0)
	{
		printk("Error %d: failed to configure interrupt on %s pin %d\n",
			   ret, button.port->name, button.pin);
		return;
	}

	gpio_init_callback(&button_cb_data, button_pressed, BIT(button.pin));
	gpio_add_callback(button.port, &button_cb_data);
	printk("Set up button at %s pin %d\n", button.port->name, button.pin);

	if (led.port && !device_is_ready(led.port))
	{
		printk("Error %d: LED device %s is not ready; ignoring it\n",
			   ret, led.port->name);
		led.port = NULL;
	}
	if (led.port)
	{
		ret = gpio_pin_configure_dt(&led, GPIO_OUTPUT);
		if (ret != 0)
		{
			printk("Error %d: failed to configure LED device %s pin %d\n",
				   ret, led.port->name, led.pin);
			led.port = NULL;
		}
		else
		{
			printk("Set up LED at %s pin %d\n", led.port->name, led.pin);
		}
	}

	timer_dev = device_get_binding(DT_LABEL(DT_ALIAS(ssm_timer)));
	counter_start(timer_dev);

	alarm_cfg.flags = 0;
	alarm_cfg.callback = counter_interrupt_fn;
	alarm_cfg.user_data = &alarm_cfg;

	PT_INIT(&generator_pt);
	PT_INIT(&button_pt);
	half_period = 500000;
	generator_thread(&generator_pt);
}
