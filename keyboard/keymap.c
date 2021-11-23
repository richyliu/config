#include QMK_KEYBOARD_H
#include "pointing_device.h"

enum layers { BASE, QWE, ARR, NAV, FUN, NUM, SYM, PLVR };

#define U_RDO SCMD(KC_Z)
#define U_PST LCMD(KC_V)
#define U_CPY LCMD(KC_C)
#define U_CUT LCMD(KC_X)
#define U_UND LCMD(KC_Z)
#define U_PSTFM LCMD(LALT(LSFT(KC_V)))

enum planck_keycodes {
  PLOVER = SAFE_RANGE,
  EXT_PLV,
  CMD_TAB,
  HEX_SLX, // sends "\x"
  HEX_ZRX, // sends "0x"
  HEX_ZZ,  // sends "00"
  MY_CAPS,
  MY_C_B
};

// use an unused keycode because of mod-tap keycode restrictions:
// https://docs.qmk.fm/#/mod_tap?id=caveats
#define S_ABSPC SFT_T(KC_PRIOR)

#define OS_ALT OSM(MOD_LALT)
#define OS_SFT OSM(MOD_LSFT)
#define OS_CTL OSM(MOD_LCTL)
#define OS_GUI OSM(MOD_LGUI)
#define CT LCTL_T
#define GT LGUI_T

#define SFT_TAB SFT_T(KC_TAB)
#define NAV_BSPC LT(NAV, KC_BSPC)
#define SYM_ENT LT(SYM, KC_ENT)

#define ALT_LEFT LALT(KC_LEFT)
#define ALT_RIGHT LALT(KC_RIGHT)

#define TAB_BAC C(S(KC_TAB))
#define TAB_FWD C(KC_TAB)

#define SCRCLIP G(C(S(KC_4)))
#define SCRSAVE G(S(KC_4))

#define INSP_EL G(A(KC_I))
#define SELC_EL G(S(KC_C))


// reference: https://beta.docs.qmk.fm/using-qmk/simple-keycodes/keycodes
const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
[BASE] = LAYOUT_ortho_4x12(
  KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    MY_C_B,  MY_C_B,  KC_J,    KC_L,    KC_U,    KC_Y,      KC_QUOT,
  KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    OS_ALT,  OS_ALT,  KC_M,    KC_N,    KC_E,    KC_I,      KC_O,
  GT(KC_Z),CT(KC_X),KC_C,    KC_D,    KC_V,    OS_CTL,  OS_CTL,  KC_K,    KC_H,    KC_COMM, CT(KC_DOT),GT(KC_SLSH),
  CMD_TAB, OS_GUI,  SFT_TAB, NAV_BSPC,OSL(NUM),OS_SFT,  OS_SFT,  SYM_ENT, KC_SPC,  S_ABSPC, XXXXXXX,   MO(FUN)
),
[QWE] = LAYOUT_ortho_4x12(
  KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    XXXXXXX, XXXXXXX, KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,
  KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    XXXXXXX, XXXXXXX, KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN,
  KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,    XXXXXXX, XXXXXXX, KC_N,    KC_M,    KC_COMM, KC_DOT,  KC_SLSH,
  XXXXXXX, KC_LALT, KC_SPC,  KC_LCTL, KC_TAB,  XXXXXXX, XXXXXXX, KC_ENT,  KC_SPC,  KC_DEL,  DF(BASE),MO(FUN)
),
[ARR] = LAYOUT_ortho_4x12(
  XXXXXXX, XXXXXXX, KC_UP,   XXXXXXX, XXXXXXX,  XXXXXXX,XXXXXXX, XXXXXXX, ALT_LEFT,XXXXXXX, XXXXXXX, ALT_RIGHT,
  XXXXXXX, KC_LEFT, KC_DOWN, KC_RGHT, XXXXXXX,  XXXXXXX,XXXXXXX, XXXXXXX, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, G(KC_GRV),XXXXXXX,XXXXXXX, XXXXXXX, KC_HOME, KC_PGDN, KC_PGUP, KC_END,
  XXXXXXX, XXXXXXX, XXXXXXX, KC_SPC,  XXXXXXX,  XXXXXXX,XXXXXXX, XXXXXXX, KC_SPC,  XXXXXXX, DF(BASE),XXXXXXX
),
[NAV] = LAYOUT_ortho_4x12(
  U_UND,   U_CUT,   U_CPY,   U_PST,   U_RDO,    XXXXXXX,XXXXXXX, XXXXXXX, ALT_LEFT,XXXXXXX, XXXXXXX, ALT_RIGHT,
  TAB_BAC, XXXXXXX, KC_ESC,  TAB_FWD, U_PSTFM,  XXXXXXX,XXXXXXX, MY_CAPS, KC_LEFT, KC_DOWN, KC_UP,   KC_RGHT,
  KC_LGUI, KC_LCTL, KC_LALT, KC_LSFT, G(KC_GRV),XXXXXXX,XXXXXXX, XXXXXXX, KC_HOME, KC_PGDN, KC_PGUP, KC_END,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,  XXXXXXX,XXXXXXX, KC_ENT,  KC_SPC,  KC_LSFT, XXXXXXX, XXXXXXX
),
[FUN] = LAYOUT_ortho_4x12(
  KC_F12,  KC_F7,   KC_F8,   KC_F9,   XXXXXXX, XXXXXXX, DM_REC2, DM_REC1, DM_RSTP, DF(QWE), PLOVER,  RESET,
  KC_F11,  KC_F4,   KC_F5,   KC_F6,   KC_BRMU, XXXXXXX, DM_PLY2, DM_PLY1, SELC_EL, INSP_EL, XXXXXXX, DF(ARR),
  KC_F10,  KC_F1,   KC_F2,   KC_F3,   KC_BRMD, XXXXXXX, XXXXXXX, XXXXXXX, KC_MUTE, KC_VOLD, KC_VOLU, XXXXXXX,
  XXXXXXX, XXXXXXX, KC_LSFT, KC_DEL,  KC_TAB,  XXXXXXX, XXXXXXX, SCRSAVE, SCRCLIP, XXXXXXX, XXXXXXX, XXXXXXX
),
[NUM] = LAYOUT_ortho_4x12(
  KC_BSPC, KC_A,    KC_B,    KC_C,    XXXXXXX, XXXXXXX, KC_LT,   KC_LBRC, KC_7,    KC_8,    KC_9,    KC_RBRC,
  KC_SPC,  KC_D,    KC_E,    KC_F,    HEX_ZRX, XXXXXXX, KC_GT,   KC_EQL,  KC_4,    KC_5,    KC_6,    KC_SCLN,
  KC_LGUI, KC_LCTL, KC_LALT, KC_LSFT, XXXXXXX, XXXXXXX, KC_SLSH, KC_BSLS, KC_1,    KC_2,    KC_3,    KC_GRV,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, KC_MINS, KC_0,    KC_DOT,  XXXXXXX, XXXXXXX
),
[SYM] = LAYOUT_ortho_4x12(
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX
),
[PLVR] = LAYOUT_ortho_4x12(
  XXXXXXX, KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
  XXXXXXX, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
  XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
  XXXXXXX, XXXXXXX, EXT_PLV, KC_1,    KC_C,    KC_V,    KC_N,    KC_M,    KC_1,    XXXXXXX, EXT_PLV, XXXXXXX
)

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
    case S_ABSPC:
      if (!record->event.pressed && record->tap.count > 0) {
        SEND_STRING(SS_LALT(SS_TAP(X_BSPC)));
      }
      return true;
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
    case HEX_SLX:
      if (record->event.pressed) {
        SEND_STRING("\\x");
      }
      return false;
    case HEX_ZRX:
      if (record->event.pressed) {
        SEND_STRING("0x");
      }
      return false;
    case HEX_ZZ:
      if (record->event.pressed) {
        SEND_STRING("00");
      }
      return false;
    case MY_CAPS:
      if (record->event.pressed) {
        tap_code_delay(KC_CAPS, 200);
      }
      return false;
    case OSL(NUM):
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
    case MY_C_B:
      // don't press ctrl-b twice if hit within a very short time frame
      if (record->event.pressed && timer_elapsed(ctrl_b_timer) > CTRL_B_TIMEOUT) {
        SEND_STRING(SS_LCTL("b"));
        ctrl_b_timer = timer_read();
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
