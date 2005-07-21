
 * This file should only contain EQUs and so on.

 * Vector addresses.
hbl_vector = $68
vbl_vector = $70
timer_b_vector = $120

 * SHIFTER addresses.
shifter_video_base_high = $FF8201
shifter_video_base_mid = $FF8203
shifter_sync_mode = $FF820A
shifter_palette = $FF8240
shifter_resolution = $FF8260

 * MFP addresses.
mfp_interrupt_enable_a = $FFFA07
mfp_interrupt_pending_a = $FFFA0B
mfp_interrupt_in_service_a = $FFFA0F
mfp_interrupt_mask_a = $FFFA13
mfp_interrupt_mask_b = $FFFA15
mfp_vector_register = $FFFA17
mfp_timer_b_control = $FFFA1B
mfp_timer_b_data = $FFFA21

 * ACIA addresses.
kbd_acia_control = $FFFC00
kbd_acia_data = $FFFC02

 * vim:syn=asm68k
