#include QMK_KEYBOARD_H
#include "print.h"

#define U_NP KC_NO // key is not present
#define U_NA KC_NO // present but not available for use
#define U_NU KC_NO // available but not used

enum layers { BASE, NOM, ARROW, NAVR, MOUR, FUNL, NSL, NSSL, _PLOVER };

#define U_RDO SCMD(KC_Z)
#define U_PST LCMD(KC_V)
#define U_CPY LCMD(KC_C)
#define U_CUT LCMD(KC_X)
#define U_UND LCMD(KC_Z)

enum planck_keycodes {
  PLOVER = SAFE_RANGE,
  EXT_PLV,
  CMD_TAB
};

// use an unused keycode because of mod-tap keycode restrictions:
// https://docs.qmk.fm/#/mod_tap?id=caveats
#define S_A_BSPC SFT_T(KC_PRIOR)


const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {
  [BASE] = LAYOUT_ortho_4x12(
    KC_Q,         KC_W,          KC_F,          KC_P,              KC_B,             U_NA,          U_NA,          KC_J,             KC_L,   KC_U,     KC_Y,           KC_QUOT,
    KC_A,         KC_R,          KC_S,          KC_T,              KC_G,             OSM(MOD_LALT), OSM(MOD_LALT), KC_M,             KC_N,   KC_E,     KC_I,           KC_O,
    LGUI_T(KC_Z), LCTL_T(KC_X),  KC_C,          KC_D,              KC_V,             OSL(MOUR),     OSL(MOUR),     KC_K,             KC_H,   KC_COMM,  LCTL_T(KC_DOT), LGUI_T(KC_SLSH),
    CMD_TAB,      OSM(MOD_LALT), SFT_T(KC_ESC), LT(NAVR, KC_BSPC), LT(NSL, KC_TAB),  OSM(MOD_LSFT), OSM(MOD_LSFT), LT(NSSL, KC_ENT), KC_SPC, S_A_BSPC, U_NP,           MO(FUNL)
  ),
  [NOM] = LAYOUT_ortho_4x12(
    KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,    U_NA,    U_NA,    KC_J,    KC_L,    KC_U,    KC_Y,     KC_QUOT,
    KC_A,    KC_R,    KC_S,    KC_T,    KC_G,    U_NA,    U_NA,    KC_M,    KC_N,    KC_E,    KC_I,     KC_O,
    KC_Z,    KC_X,    KC_C,    KC_D,    KC_V,    U_NA,    U_NA,    KC_K,    KC_H,    KC_COMM, KC_DOT,   KC_SLSH,
    U_NP,    U_NP,    KC_ESC,  KC_BSPC, KC_TAB,  U_NA,    U_NA,    KC_ENT,  KC_SPC,  KC_DEL,  DF(BASE), U_NP
  ),
  [ARROW] = LAYOUT_ortho_4x12(
    U_NA,    U_NA,    KC_UP,   U_NA,    U_NA,    U_NA,    U_NA,    U_NA,    LALT(KC_LEFT), U_NA,    U_NA,     LALT(KC_RIGHT),
    U_NA,    KC_LEFT, KC_DOWN, KC_RGHT, U_NA,    U_NA,    U_NA,    U_NA,    KC_LEFT,       KC_DOWN, KC_UP,    KC_RGHT,
    U_NA,    U_NA,    U_NA,    U_NA,    U_NA,    U_NA,    U_NA,    U_NA,    KC_PGDN,       KC_PGUP, KC_HOME,  KC_END,
    U_NP,    U_NP,    U_NA,    KC_SPC,  U_NA,    U_NA,    U_NA,    U_NA,    KC_SPC,        U_NA,    DF(BASE), U_NP
  ),
  [NAVR] = LAYOUT_ortho_4x12(
    U_UND,        U_CUT,   U_CPY,   U_PST,     U_RDO, KC_NO, KC_NO, U_NA,    LALT(KC_LEFT), U_NA,    U_NA,     LALT(KC_RIGHT),
    C(S(KC_TAB)), U_NA,    U_NA,    C(KC_TAB), U_NA,  KC_NO, KC_NO, KC_CAPS, KC_LEFT,       KC_DOWN, KC_UP,    KC_RGHT,
    KC_LGUI,      KC_LCTL, U_NA,    U_NA,      U_NA,  KC_NO, KC_NO, U_NA,    KC_PGDN,       KC_PGUP, KC_HOME,  KC_END,
    U_NP,         U_NP,    U_NA,    U_NA,      U_NA,  KC_NO, KC_NO, KC_ENT,  KC_SPC,        KC_LSFT, U_NP,     U_NP
  ),
  [MOUR] = LAYOUT_ortho_4x12(
    U_NA,    U_NA,    U_NA,    U_NA,    U_NA,    KC_NO, KC_NO, U_NU,    U_NU,    U_NU,    U_NU,    U_NU,
    U_NA,    U_NA,    U_NA,    U_NA,    U_NA,    KC_NO, KC_NO, U_NU,    KC_MS_L, KC_MS_D, KC_MS_U, KC_MS_R,
    KC_LGUI, KC_LCTL, U_NA,    U_NA,    U_NA,    KC_NO, KC_NO, U_NU,    KC_WH_L, KC_WH_D, KC_WH_U, KC_WH_R,
    U_NP,    U_NP,    U_NA,    U_NA,    U_NA,    KC_NO, KC_NO, KC_BTN1, KC_BTN3, KC_BTN2, U_NP,    U_NP
  ),
  [FUNL] = LAYOUT_ortho_4x12(
    KC_F12,  KC_F7,   KC_F8,   KC_F9,   U_NU,    KC_NO, KC_NO, U_NA,    DF(ARROW), DF(NOM), PLOVER,  RESET,
    KC_F11,  KC_F4,   KC_F5,   KC_F6,   KC_BRMU, KC_NO, KC_NO, U_NA,    U_NA,      U_NA,    U_NA,    U_NA,
    KC_F10,  KC_F1,   KC_F2,   KC_F3,   KC_BRMD, KC_NO, KC_NO, U_NA,    KC_MUTE,   KC_VOLD, KC_VOLU, U_NU,
    U_NP,    U_NP,    KC_LSFT, KC_DEL,  KC_TAB,  KC_NO, KC_NO, U_NA,    U_NA,      U_NA,    U_NP,    U_NP
  ),
  [NSL] = LAYOUT_ortho_4x12(
    U_NA,    U_NA,    U_NA,    U_NA,    U_NA,    KC_NO, KC_NO, KC_LBRC, KC_7,    KC_8,    KC_9,    KC_RBRC,
    U_NA,    U_NA,    U_NA,    U_NA,    U_NA,    KC_NO, KC_NO, KC_EQL,  KC_4,    KC_5,    KC_6,    KC_COLN,
    KC_LGUI, KC_LCTL, U_NA,    U_NA,    U_NA,    KC_NO, KC_NO, KC_BSLS, KC_1,    KC_2,    KC_3,    KC_GRV,
    U_NP,    U_NP,    U_NA,    U_NA,    U_NA,    KC_NO, KC_NO, KC_MINS, KC_0,    KC_DOT,  U_NP,    U_NP
  ),
  [NSSL] = LAYOUT_ortho_4x12(
    KC_LCBR, KC_AMPR, KC_ASTR, KC_LPRN, KC_RCBR, KC_NO, KC_NO, DM_REC2, DM_REC1, DM_RSTP, DM_PLY1, DM_PLY2,
    KC_SCLN, KC_DLR,  KC_PERC, KC_CIRC, KC_PLUS, KC_NO, KC_NO, U_NA,    U_NA,    U_NA,    U_NA,    U_NA,
    KC_TILD, KC_EXLM, KC_AT,   KC_HASH, KC_PIPE, KC_NO, KC_NO, U_NA,    U_NA,    U_NA,    KC_LCTL, KC_LGUI,
    U_NP,    U_NP,    KC_LSFT, KC_RPRN, KC_UNDS, KC_NO, KC_NO, U_NA,    U_NA,    U_NA,    U_NP,    U_NP
  ),

/* Plover layer (http://opensteno.org)
 * ,-----------------------------------------------------------------------------------.
 * |      |   S  |   T  |   P  |   H  |   *  |   *  |   F  |   P  |   L  |   T  |   D  |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |   S  |   K  |   W  |   R  |   *  |   *  |   R  |   B  |   G  |   S  |   Z  |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |      |      |      |      |      |      |      |      |      |      |      |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * | Exit |      |      |   #  |   A  l   O  |   E  |   U  |   #  |      |      |      |
 * `-----------------------------------------------------------------------------------'
 */
[_PLOVER] = LAYOUT_ortho_4x12(
    XXXXXXX, KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
    XXXXXXX, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
    XXXXXXX, XXXXXXX, EXT_PLV, KC_1,    KC_C,    KC_V,    KC_N,    KC_M,    KC_1,    XXXXXXX, EXT_PLV, XXXXXXX
)

};

bool is_cmd_tab_active = false;
uint16_t cmd_tab_timer = 0;
#define CMD_TAB_TIMEOUT 700

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  dprintf("keycode: %04x\n", keycode);
  switch (keycode) {
    case PLOVER:
      if (record->event.pressed) {
        layer_move(_PLOVER);
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
    case S_A_BSPC:
      if (!record->event.pressed && record->tap.count > 0) {
        SEND_STRING(SS_LALT(SS_TAP(X_BSPC)));
      }
      return true;
    case CMD_TAB:
      /* "super alt tab" (source: https://docs.qmk.fm/#/feature_macros?id=super-alt↯tab) */
      if (record->event.pressed) {
        if (!is_cmd_tab_active) {
          is_cmd_tab_active = true;
          register_code(KC_LGUI);
        }
        cmd_tab_timer = timer_read();
        register_code(KC_TAB);
      } else {
        unregister_code(KC_TAB);
      }
      return false;
  }
  return true;
}

void matrix_scan_user(void) {
  if (is_cmd_tab_active) {
    if (timer_elapsed(cmd_tab_timer) > CMD_TAB_TIMEOUT) {
      unregister_code(KC_LGUI);
      is_cmd_tab_active = false;
    }
  }
}

void keyboard_post_init_user(void) {
  // Customise these values to desired behaviour
  debug_enable=true;
  debug_matrix=true;
  debug_keyboard=true;
  //debug_mouse=true;
}
