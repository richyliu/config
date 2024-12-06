#include QMK_KEYBOARD_H
#include "version.h"

#define KC_MAC_UNDO LGUI(KC_Z)
#define KC_MAC_CUT LGUI(KC_X)
#define KC_MAC_COPY LGUI(KC_C)
#define KC_MAC_PASTE LGUI(KC_V)
#define KC_PC_UNDO LCTL(KC_Z)
#define KC_PC_CUT LCTL(KC_X)
#define KC_PC_COPY LCTL(KC_C)
#define KC_PC_PASTE LCTL(KC_V)
#define ES_LESS_MAC KC_GRAVE
#define ES_GRTR_MAC LSFT(KC_GRAVE)
#define ES_BSLS_MAC ALGR(KC_6)
#define NO_PIPE_ALT KC_GRAVE
#define NO_BSLS_ALT KC_EQUAL
#define LSA_T(kc) MT(MOD_LSFT | MOD_LALT, kc)
#define BP_NDSH_MAC ALGR(KC_8)
#define SE_SECT_MAC ALGR(KC_6)

enum custom_keycodes {
  RGB_SLD = SAFE_RANGE,
  PLOVER,
  EXIT_PLOVER,
  CMD_TAB,
  CTRL_TAB,
};

#define KC_PASTE_NO_FMT LALT(LGUI(LSFT(KC_V)))
#define KC_SCRSHOT      LGUI(LSFT(KC_4))
#define KC_SCRSHOT_CLIP LGUI(LCTL(LSFT(KC_4)))
#define KC_SEL_EL       LGUI(LSFT(KC_X))
#define KC_INSP_EL      LALT(LGUI(KC_L))
#define KC_WEB_BACK     LGUI(KC_LBRC)
#define KC_WEB_FWD      LGUI(KC_RBRC)

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [0] = LAYOUT_ergodox_pretty(
    KC_ESC,         KC_1,           KC_2,           KC_3,           KC_4,           KC_5,           LSFT(KC_LALT),                                  LSFT(KC_LCTL),  KC_6,           KC_7,           KC_8,           KC_9,           KC_0,           KC_TRANSPARENT,
    KC_TAB,         KC_Q,           KC_W,           KC_E,           KC_R,           KC_T,           KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_Y,           KC_U,           KC_I,           KC_O,           KC_P,           KC_BSPC,
    KC_LCTL,        KC_A,           KC_S,           KC_D,           KC_F,           KC_G,                                                                           KC_H,           KC_J,           KC_K,           KC_L,           KC_SCLN,        KC_ENTER,
    KC_LSFT,        KC_B,           KC_Z,           KC_X,           KC_C,           KC_V,           CTRL_TAB,                                       LCTL(LSFT(KC_TAB)),KC_N,        KC_M,           KC_COMMA,       KC_DOT,         KC_SLASH,       MT(MOD_RSFT, KC_QUOTE),
    MO(2),          LALT(KC_LCTL),  KC_TRANSPARENT, KC_SPACE,       KC_LALT,                                                                                                        KC_LEFT,        KC_DOWN,        KC_UP,          KC_RIGHT,       MO(2),
                                                                                                    LGUI(KC_LALT),  KC_HOME,        KC_PGUP,        KC_RALT,
                                                                                                                    KC_END,         KC_PGDN,
                                                                                    KC_LGUI,        MO(1),          CMD_TAB,        CMD_TAB,        MO(1),          KC_SPACE
  ),
  [1] = LAYOUT_ergodox_pretty(
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_BSLS,
    KC_TRANSPARENT, KC_EXLM,        KC_AT,          KC_HASH,        KC_DLR,         KC_PERC,        KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_CIRC,        KC_AMPR,        KC_ASTR,        KC_LCBR,        KC_RCBR,        KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_GRAVE,       KC_BSLS,                                                                        KC_TRANSPARENT, KC_MINUS,       KC_EQUAL,       KC_LBRC,        KC_RBRC,        KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TILDE,       KC_PIPE,        KC_WEB_BACK,                                    KC_WEB_FWD,     KC_TRANSPARENT, KC_UNDS,        KC_PLUS,        KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                                                                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT
  ),
  [2] = LAYOUT_ergodox_pretty(
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,     KC_TRANSPARENT,  KC_TRANSPARENT,    PLOVER,          QK_BOOT,
    KC_TRANSPARENT, KC_F12,         KC_F7,          KC_F8,          KC_F9,          KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_PASTE_NO_FMT,    KC_TRANSPARENT,  KC_TRANSPARENT,    LED_LEVEL,       KC_DELETE,
    KC_TRANSPARENT, KC_F11,         KC_F4,          KC_F5,          KC_F6,          KC_TRANSPARENT,                                                                 KC_CAPS,        KC_BRIGHTNESS_UP,   KC_SEL_EL,       KC_INSP_EL,        KC_TRANSPARENT,  KC_TRANSPARENT,
    KC_TRANSPARENT, KC_F10,         KC_F1,          KC_F2,          KC_F3,          KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_BRIGHTNESS_DOWN, KC_AUDIO_MUTE,   KC_AUDIO_VOL_DOWN, KC_AUDIO_VOL_UP, KC_TRANSPARENT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                                                                                 KC_SCRSHOT,         KC_SCRSHOT_CLIP, KC_TRANSPARENT,    KC_TRANSPARENT,  KC_TRANSPARENT,
                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT
  ),
  [3] = LAYOUT_ergodox_pretty(
    KC_TRANSPARENT, KC_1,           KC_2,           KC_3,           KC_4,           KC_5,           KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_6,           KC_7,           KC_8,           KC_9,           KC_0,           KC_TRANSPARENT,
    KC_TRANSPARENT, KC_Q,           KC_W,           KC_E,           KC_R,           KC_T,           KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_Y,           KC_U,           KC_I,           KC_O,           KC_P,           KC_LBRC,
    KC_TRANSPARENT, KC_A,           KC_S,           KC_D,           KC_F,           KC_G,                                                                           KC_H,           KC_J,           KC_K,           KC_L,           KC_SCLN,        KC_QUOT,
    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,                                 KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
    EXIT_PLOVER,    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_1,                                                                                                           KC_1,           KC_TRANSPARENT, KC_TRANSPARENT, EXIT_PLOVER,    KC_TRANSPARENT,
                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                                                    KC_TRANSPARENT, KC_TRANSPARENT,
                                                                                    KC_C,           KC_V,           KC_TRANSPARENT, KC_TRANSPARENT, KC_N,           KC_M
  ),
};


/**
 * "*-tab" key, as adapted from "super alt tab":
 *    https://docs.qmk.fm/#/feature_macros?id=super-alt↯tab
 * (where * is either cmd or ctrl)
 *
 * When the key is tapped, a single *-tab key is tapped, switching to the
 * most recently used application (timeout set by TAPPING_TERM). When the key
 * is held, it gives the user a set time to view the applications (as
 * determined by TAB_BEFORE_FIRST_TIMEOUT). The is_hold bool is set
 * during the first hold and the is_before_first is set after the hold
 * but before the first tap after the hold. Once a key is tapped,
 * is_active will be set. Each key tap will go to the next application,
 * and after the timeout (TAB_TIMEOUT) it will stop at the current
 * application.
 */
#define TAB_TIMEOUT 500
#define TAB_BEFORE_FIRST_TIMEOUT 1500
struct tab_state {
  bool is_active;
  uint16_t timer;
  bool is_hold;
  uint16_t hold_timer;
  bool is_before_first;
  uint16_t before_first_timer;
};

struct tab_state cmd_tab_state;
struct tab_state ctrl_tab_state;


bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
    case KC_BSPC:
      // send esc on ctrl-backspace
      if (get_mods() & MOD_BIT(KC_LCTL)) {
        if (record->event.pressed) {
          // don't want ctrl to be sent along with esc
          unregister_code(KC_LCTL);
          tap_code(KC_ESC);
          wait_ms(10); // wait for the key to be processed
          register_code(KC_LCTL);
        }
        // don't send the original keycode
        return false;
      }
      break;
    case PLOVER:
      if (record->event.pressed) {
        // stay in plover layer until exit_plover is pressed
        layer_move(3);
        // start plover/plojo with ctrl-alt-shift-p
        SEND_STRING(SS_LSFT(SS_TAP(X_F7)));
      }
      return false;
    case EXIT_PLOVER:
      if (record->event.pressed) {
        // exit plover layer
        layer_move(0);
        // exit plover/plojo with "PHR*OF" stroke (erfvyu keys)
        // all the keys must be pressed down at the same time to simulate a steno chord
        SEND_STRING(SS_DOWN(X_E) SS_DOWN(X_R) SS_DOWN(X_F) SS_DOWN(X_V) SS_DOWN(X_Y) SS_DOWN(X_U));
        SEND_STRING(SS_UP(X_E) SS_UP(X_R) SS_UP(X_F) SS_UP(X_V) SS_UP(X_Y) SS_UP(X_U));
      }
      return false;
    case CMD_TAB:
      // reset ctrl-tab if cmd-tab is pressed
      unregister_code(KC_LCTL);
      memset(&ctrl_tab_state, 0, sizeof(ctrl_tab_state));

      if (record->event.pressed) {
        if (!cmd_tab_state.is_active) {
          cmd_tab_state.is_hold = true;
          cmd_tab_state.hold_timer = timer_read();
        } else {
          cmd_tab_state.is_before_first = false;
          cmd_tab_state.timer = timer_read();
          register_code(KC_TAB);
        }
      } else {
        if (cmd_tab_state.is_hold) {
          if (timer_elapsed(cmd_tab_state.hold_timer) < TAPPING_TERM) {
            register_code(KC_LGUI);
            tap_code(KC_TAB);
            unregister_code(KC_LGUI);
            cmd_tab_state.is_hold = false;
          }
        }
        if (cmd_tab_state.is_active) {
          unregister_code(KC_TAB);
        }
      }
      return false;
    case CTRL_TAB:
      // reset cmd-tab if ctrl-tab is pressed
      unregister_code(KC_LGUI);
      memset(&cmd_tab_state, 0, sizeof(cmd_tab_state));

      if (record->event.pressed) {
        if (!ctrl_tab_state.is_active) {
          ctrl_tab_state.is_hold = true;
          ctrl_tab_state.hold_timer = timer_read();
        } else {
          ctrl_tab_state.is_before_first = false;
          ctrl_tab_state.timer = timer_read();
          register_code(KC_TAB);
        }
      } else {
        if (ctrl_tab_state.is_hold) {
          if (timer_elapsed(ctrl_tab_state.hold_timer) < TAPPING_TERM) {
            register_code(KC_LCTL);
            tap_code(KC_TAB);
            unregister_code(KC_LCTL);
            ctrl_tab_state.is_hold = false;
          }
        }
        if (ctrl_tab_state.is_active) {
          unregister_code(KC_TAB);
        }
      }
      return false;
  }

  /// NOTE: important to check this after checking for cmd-tab key
  // reset cmd-tab if a command modifier was released
  if ((keycode >> 8) & MOD_LGUI && !record->event.pressed) {
    cmd_tab_state.is_active = false;
    cmd_tab_state.is_before_first = false;
  }
  if ((keycode >> 8) & MOD_LCTL && !record->event.pressed) {
    ctrl_tab_state.is_active = false;
    ctrl_tab_state.is_before_first = false;
  }

   return true;
}

void matrix_scan_user(void) {
  if (cmd_tab_state.is_before_first) {
    if (timer_elapsed(cmd_tab_state.before_first_timer) > TAB_BEFORE_FIRST_TIMEOUT) {
      unregister_code(KC_LGUI);
      cmd_tab_state.is_before_first = false;
      cmd_tab_state.is_active = false;
    }
  } else {
    if (cmd_tab_state.is_active) {
      if (timer_elapsed(cmd_tab_state.timer) > TAB_TIMEOUT) {
        unregister_code(KC_LGUI);
        cmd_tab_state.is_active = false;
      }
    }
  }
  if (cmd_tab_state.is_hold) {
    if (timer_elapsed(cmd_tab_state.hold_timer) > TAPPING_TERM) {
      register_code(KC_LGUI);
      tap_code(KC_TAB);
      cmd_tab_state.is_hold = false;
      cmd_tab_state.is_active = true;
      cmd_tab_state.is_before_first = true;
      cmd_tab_state.before_first_timer = timer_read();
    }
  }
  if (ctrl_tab_state.is_before_first) {
    if (timer_elapsed(ctrl_tab_state.before_first_timer) > TAB_BEFORE_FIRST_TIMEOUT) {
      unregister_code(KC_LCTL);
      ctrl_tab_state.is_before_first = false;
      ctrl_tab_state.is_active = false;
    }
  } else {
    if (ctrl_tab_state.is_active) {
      if (timer_elapsed(ctrl_tab_state.timer) > TAB_TIMEOUT) {
        unregister_code(KC_LCTL);
        ctrl_tab_state.is_active = false;
      }
    }
  }
  if (ctrl_tab_state.is_hold) {
    if (timer_elapsed(ctrl_tab_state.hold_timer) > TAPPING_TERM) {
      register_code(KC_LCTL);
      tap_code(KC_TAB);
      ctrl_tab_state.is_hold = false;
      ctrl_tab_state.is_active = true;
      ctrl_tab_state.is_before_first = true;
      ctrl_tab_state.before_first_timer = timer_read();
    }
  }
}

uint8_t layer_state_set_user(uint8_t state) {
    uint8_t layer = biton(state);
  ergodox_board_led_off();
  ergodox_right_led_1_off();
  ergodox_right_led_2_off();
  ergodox_right_led_3_off();
  switch (layer) {
    case 1:
      ergodox_right_led_1_on();
      break;
    case 2:
      ergodox_right_led_2_on();
      break;
    case 3:
      ergodox_right_led_3_on();
      break;
    case 4:
      ergodox_right_led_1_on();
      ergodox_right_led_2_on();
      break;
    case 5:
      ergodox_right_led_1_on();
      ergodox_right_led_3_on();
      break;
    case 6:
      ergodox_right_led_2_on();
      ergodox_right_led_3_on();
      break;
    case 7:
      ergodox_right_led_1_on();
      ergodox_right_led_2_on();
      ergodox_right_led_3_on();
      break;
    default:
      break;
  }
  return state;
};

