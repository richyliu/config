#include QMK_KEYBOARD_H
#include "pointing_device.h"

#include <stdint.h>
#include <stdbool.h>

enum layers { BASE, ARR, NAV, FUN, NUM, PLVR };

#define U_PSTFM LCMD(LALT(LSFT(KC_V)))

enum planck_keycodes {
  PLOVER = SAFE_RANGE,
  EXT_PLV,
  CMD_TAB,
  HEX_0X, // sends "0x"
  MY_CAPS
};

// correspond to the correct letters on colemak mod-dh
#define HEX_A KC_A
#define HEX_B KC_T
#define HEX_C KC_X
#define HEX_D KC_C
#define HEX_E KC_K
#define HEX_F KC_E
#define HEX_X KC_Z // for sending "0x"

#define ALT_LEFT LALT(KC_LEFT)
#define ALT_RIGHT LALT(KC_RIGHT)

#define ALT_ESC LALT_T(KC_ESC)
#define SFT_QUOT LSFT_T(KC_QUOT)

#define TAB_BAC G(S(KC_LBRC))
#define TAB_FWD G(S(KC_RBRC))

#define SCRCLIP G(C(S(KC_4)))
#define SCRSAVE G(S(KC_4))

#define INSP_EL G(A(KC_L))
#define SELC_EL G(S(KC_X))

// TODO:
// layer order/DF layer stacking?
// add sound?
// add lights?

// reference: https://docs.qmk.fm/#/keycodes
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
[BASE] = LAYOUT_ortho_4x12(
  KC_TAB,  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_BSPC,
  KC_LCTL, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_ENT,
  KC_LSFT, KC_B,    KC_Z,    KC_X,    KC_C,    KC_V,    KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH, SFT_QUOT,
  XXXXXXX, MO(NAV), CMD_TAB, ALT_ESC, KC_LCMD, MO(NUM), MO(NUM), KC_SPC,  MO(NAV), MO(FUN), DF(BASE),MO(FUN)
),
[ARR] = LAYOUT_ortho_4x12(
  XXXXXXX, XXXXXXX, XXXXXXX, KC_UP,   XXXXXXX, XXXXXXX, XXXXXXX, ALT_LEFT,XXXXXXX, XXXXXXX, ALT_RIGHT,XXXXXXX,
  XXXXXXX, XXXXXXX, KC_LEFT, KC_DOWN, KC_RGHT, XXXXXXX, XXXXXXX, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, XXXXXXX,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_HOME, KC_PGDN, KC_PGUP, KC_END,  XXXXXXX,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_SPC,  XXXXXXX, XXXXXXX, KC_SPC,  XXXXXXX, XXXXXXX, _______, _______
),
[NAV] = LAYOUT_ortho_4x12(
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, ALT_LEFT,XXXXXXX, XXXXXXX, ALT_RIGHT,KC_DEL,
  XXXXXXX, XXXXXXX, XXXXXXX, TAB_BAC, TAB_FWD, U_PSTFM, MY_CAPS, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT, KC_ENT,
  _______, KC_LGUI, KC_LALT, KC_LCTL, KC_LSFT, XXXXXXX, XXXXXXX, KC_HOME, KC_PGDN, KC_PGUP, KC_END,  KC_LSFT,
  XXXXXXX, XXXXXXX, XXXXXXX, KC_LALT, _______, XXXXXXX, XXXXXXX, _______, XXXXXXX, XXXXXXX, _______, _______
),
[FUN] = LAYOUT_ortho_4x12(
  XXXXXXX, KC_F12,  KC_F7,   KC_F8,   KC_F9,   XXXXXXX, DM_REC2, DM_REC1, DM_RSTP, XXXXXXX, PLOVER,  RESET,
  XXXXXXX, KC_F11,  KC_F4,   KC_F5,   KC_F6,   KC_BRMU, DM_PLY2, DM_PLY1, SELC_EL, INSP_EL, XXXXXXX, DF(ARR),
  _______, KC_F10,  KC_F1,   KC_F2,   KC_F3,   KC_BRMD, XXXXXXX, XXXXXXX, KC_MUTE, KC_VOLD, KC_VOLU, KC_LSFT,
  XXXXXXX, XXXXXXX, XXXXXXX, KC_LALT, _______, XXXXXXX, XXXXXXX, SCRSAVE, SCRCLIP, XXXXXXX, _______, _______
),
[NUM] = LAYOUT_ortho_4x12(
  KC_GRV,  KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_BSPC,
  XXXXXXX, HEX_A,   HEX_B,   HEX_C,   HEX_D,   HEX_E,   HEX_F,   KC_MINS, KC_EQL,  KC_LBRC, KC_RBRC, KC_BSLS,
  KC_LSFT, KC_LCMD, KC_LALT, KC_LCTL, KC_LSFT, HEX_0X,  XXXXXXX, KC_RSFT, KC_RCTL, KC_LALT, KC_RGUI, KC_RSFT,
  XXXXXXX, XXXXXXX, XXXXXXX, _______, _______, XXXXXXX, XXXXXXX, _______, KC_COMM, XXXXXXX, _______, _______
),
[PLVR] = LAYOUT_ortho_4x12(
  XXXXXXX, KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
  XXXXXXX, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  XXXXXXX, XXXXXXX, EXT_PLV, KC_1,    KC_C,    KC_V,    KC_N,    KC_M,    KC_1,    XXXXXXX, EXT_PLV, XXXXXXX
)

};


const key_override_t esc_key_override = ko_make_basic(MOD_MASK_CTRL, KC_BSPC, KC_ESC);

const key_override_t **key_overrides = (const key_override_t *[]){
    &esc_key_override,
    NULL
};


/**
 * "cmd-tab" key, as adapted from "super alt tab":
 *    https://docs.qmk.fm/#/feature_macros?id=super-alt↯tab
 *
 * When the key is tapped, a single cmd-tab key is tapped, switching to the
 * most recently used application (timeout set by TAPPING_TERM). When the key
 * is held, it gives the user a set time to view the applications (as
 * determined by CMD_TAB_BEFORE_FIRST_TIMEOUT). The is_cmd_tab_hold bool is set
 * during the first hold and the is_cmd_tab_before_first is set after the hold
 * but before the first tap after the hold. Once a key is tapped,
 * is_cmd_tab_active will be set. Each key tap will go to the next application,
 * and after the timeout (CMD_TAB_TIMEOUT) it will stop at the current
 * application.
 */
bool is_cmd_tab_active = false;
uint16_t cmd_tab_timer = 0;
#define CMD_TAB_TIMEOUT 500
bool is_cmd_tab_hold = false;
uint16_t cmd_tab_hold_timer = 0;
bool is_cmd_tab_before_first = false;
uint16_t cmd_tab_before_first_timer = 0;
#define CMD_TAB_BEFORE_FIRST_TIMEOUT 3000

bool is_shift_down = false;
bool is_num_layer = false;

uint16_t ctrl_b_timer = 0;
#define CTRL_B_TIMEOUT 300

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
    case PLOVER:
      if (record->event.pressed) {
        layer_move(PLVR);
        // start plover/plojo with ctrl-alt-shift-p
        SEND_STRING(SS_LSFT(SS_TAP(X_F7)));
      }
      return false;
    case EXT_PLV:
      if (record->event.pressed) {
        layer_move(BASE);
        // exit plover/plojo with "PHR*OF" stroke (erfvyu keys)
        // all the keys must be pressed down at the same time to simulate a steno chord
        SEND_STRING(SS_DOWN(X_E) SS_DOWN(X_R) SS_DOWN(X_F) SS_DOWN(X_V) SS_DOWN(X_Y) SS_DOWN(X_U));
        SEND_STRING(SS_UP(X_E) SS_UP(X_R) SS_UP(X_F) SS_UP(X_V) SS_UP(X_Y) SS_UP(X_U));
      }
      return false;
    case CMD_TAB:
      if (record->event.pressed) {
        if (!is_cmd_tab_active) {
          is_cmd_tab_hold = true;
          cmd_tab_hold_timer = timer_read();
        } else {
          is_cmd_tab_before_first = false;
          cmd_tab_timer = timer_read();
          register_code(KC_TAB);
        }
      } else {
        if (is_cmd_tab_hold) {
          if (timer_elapsed(cmd_tab_hold_timer) < TAPPING_TERM) {
            register_code(KC_LGUI);
            tap_code(KC_TAB);
            unregister_code(KC_LGUI);
            is_cmd_tab_hold = false;
          }
        }
        if (is_cmd_tab_active) {
          unregister_code(KC_TAB);
        }
      }
      return false;
    case HEX_0X:
      if (record->event.pressed) {
        tap_code(KC_0);
        tap_code(HEX_X);
      }
      return false;
    case MY_CAPS:
      if (record->event.pressed) {
        tap_code_delay(KC_CAPS, 200);
      }
      return false;
    case KC_TAB:
      is_num_layer = record->event.pressed;
      break;
    case KC_LSFT:
      is_shift_down = record->event.pressed;
      break;
    case KC_DOT:
      // press comma (,) when on num layer and pressing shift
      if (!is_num_layer || !is_shift_down)
        break;
      if (record->event.pressed) {
        unregister_code(KC_LSFT);
        register_code(KC_COMM);
      } else {
        unregister_code(KC_COMM);
        register_code(KC_LSFT);
      }
      return false;
  }

  /// NOTE: important to check this after checking for cmd-tab key
  // reset cmd-tab if a command modifier was released
  if ((keycode >> 8) & MOD_LGUI && !record->event.pressed) {
    is_cmd_tab_active = false;
    is_cmd_tab_before_first = false;
  }

  return true;
}

void matrix_scan_user(void) {
  if (is_cmd_tab_before_first) {
    if (timer_elapsed(cmd_tab_before_first_timer) > CMD_TAB_BEFORE_FIRST_TIMEOUT) {
      unregister_code(KC_LGUI);
      is_cmd_tab_before_first = false;
      is_cmd_tab_active = false;
    }
  } else {
    if (is_cmd_tab_active) {
      if (timer_elapsed(cmd_tab_timer) > CMD_TAB_TIMEOUT) {
        unregister_code(KC_LGUI);
        is_cmd_tab_active = false;
      }
    }
  }
  if (is_cmd_tab_hold) {
    if (timer_elapsed(cmd_tab_hold_timer) > TAPPING_TERM) {
      register_code(KC_LGUI);
      tap_code(KC_TAB);
      is_cmd_tab_hold = false;
      is_cmd_tab_active = true;
      is_cmd_tab_before_first = true;
      cmd_tab_before_first_timer = timer_read();
    }
  }
}
