/* Copyright 2015-2017 Jack Humbert
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include QMK_KEYBOARD_H
#include "muse.h"

#define RETRO_TAPPING_PER_KEY

enum planck_layers {
  _COLEMAK,
  _QWERTY,
  _FUNC,
  _LOWER,
  _RAISE,
  _PLOVER,
  _ADJUST,
  _NAV,
};

enum planck_keycodes {
  QWERTY = SAFE_RANGE,
  COLEMAK,
  PLOVER,
  EXT_PLV
};

const uint16_t PROGMEM keymaps[][MATRIX_ROWS][MATRIX_COLS] = {

/* Colemak
 * ,-----------------------------------------------------------------------------------.
 * | Tab  |   Q  |   W  |   F  |   P  |   B  |   J  |   L  |   U  |   Y  |   ;  | Bksp |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * | Esc* |   A  |   R  |   S  |   T  |   G  |   H  |   N  |   E  |   I  |   O  |  '*  |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * | Shift|   Z  |   X  |   C  |   D  |   V  |   K  |   M  |   ,  |   .  |   /  |Enter |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * | Ctrl | Nav  |      | Alt  | Cmd  |Lower |Space |Raise |Shift | Func | Cfg  |      |
 * `-----------------------------------------------------------------------------------'
 *   *: Esc is ctrl when held
 *   *: Quote is Nav when held
 */
[_COLEMAK] = LAYOUT_ortho_4x12(
    KC_TAB,         KC_Q,    KC_W,    KC_F,    KC_P,    KC_B,       KC_J,    KC_L,         KC_U,    KC_Y,      KC_SCLN,     KC_BSPC,
    LCTL_T(KC_ESC), KC_A,    KC_R,    KC_S,    KC_T,    KC_G,       KC_M,    KC_N,         KC_E,    KC_I,      KC_O,        LT(_NAV,KC_QUOT),
    KC_LSFT,        KC_Z,    KC_X,    KC_C,    KC_D,    KC_V,       KC_K,    KC_H,         KC_COMM, KC_DOT,    KC_SLSH,     KC_ENT,
    KC_LCTL,        MO(_NAV),_______, KC_LALT, KC_LGUI, MO(_LOWER), KC_SPC,  MO(_RAISE),   KC_RSFT, MO(_FUNC), MO(_ADJUST), _______
),

/* Qwerty
 * ,-----------------------------------------------------------------------------------.
 * | Tab  |   Q  |   W  |   E  |   R  |   T  |   Y  |   U  |   I  |   O  |   P  | Bksp |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * | Esc* |   A  |   S  |   D  |   F  |   G  |   H  |   J  |   K  |   L  |   ;  |  '*  |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * | Shift|   Z  |   X  |   C  |   V  |   B  |   N  |   M  |   ,  |   .  |   /  |Enter |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * | Ctrl | Nav  |      | Alt  | Cmd  |Lower |Space |Raise |Shift | Func | Cfg  |      |
 * `-----------------------------------------------------------------------------------'
 *   *: Esc is ctrl when held
 *   *: Quote is Nav when held
 */
[_QWERTY] = LAYOUT_ortho_4x12(
    KC_TAB,         KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,       KC_Y,    KC_U,         KC_I,    KC_O,      KC_P,        KC_BSPC,
    LCTL_T(KC_ESC), KC_A,    KC_S,    KC_D,    KC_F,    KC_G,       KC_H,    KC_J,         KC_K,    KC_L,      KC_SCLN,     LT(_NAV,KC_QUOT),
    KC_LSFT,        KC_Z,    KC_X,    KC_C,    KC_V,    KC_B,       KC_N,    KC_M,         KC_COMM, KC_DOT,    KC_SLSH,     KC_ENT,
    KC_LCTL,        MO(_NAV),_______, KC_LALT, KC_LGUI, MO(_LOWER), KC_SPC,  MO(_RAISE),   KC_RSFT, MO(_FUNC), MO(_ADJUST), _______
),

/* Func
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |  F1  |  F2  |  F3  |  F4  |  F5  |      |      |      |      |      | Del  |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |  F6  |  F7  |  F8  |  F9  |  F10 |      |      |      |      |      |      |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |  F11 |  F12 |      |      |      |      |      |      |      |      |      |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |      |      |      |      |      |      |      |      | Func |      |      |
 * `-----------------------------------------------------------------------------------'
 *   *: Esc is ctrl when held
 *   *: Quote is Nav when held
 */
[_FUNC] = LAYOUT_ortho_4x12(
    _______, KC_F1,   KC_F2,   KC_F3,   KC_F4,   KC_F5,   _______, _______, _______, _______, _______, KC_DEL,
    _______, KC_F6,   KC_F7,   KC_F8,   KC_F9,   KC_F10,  _______, _______, _______, _______, _______, _______,
    _______, KC_F11,  KC_F12,  _______, _______, _______, _______, _______, _______, _______, _______, _______,
    _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______, _______
),

/* Lower
 * ,-----------------------------------------------------------------------------------.
 * |      |   !  |   @  |   {  |   }  |   `  |   /  | A-L  |  Up  | A-R  | Home |A-Bksp|
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |   #  |   $  |   (  |   )  |   ~  |   |  | Left | Down | Right| End  |      |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |   %  |   &  |   [  |   ]  |   _  |   \  |Cmd-L |Cmd-R | PgUp | PgDn |      |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |      |      |      |      |Lower |      |      |      |      |      |      |
 * `-----------------------------------------------------------------------------------'
 */
[_LOWER] = LAYOUT_ortho_4x12(
    _______, KC_EXLM, KC_AT,   KC_LCBR, KC_RCBR, KC_GRV,  KC_SLSH, LALT(KC_LEFT), KC_UP,       LALT(KC_RGHT), KC_HOME, A(KC_BSPC),
    _______, KC_HASH, KC_DLR,  KC_LPRN, KC_RPRN, KC_TILD, KC_PIPE, KC_LEFT,       KC_DOWN,     KC_RGHT,       KC_END, _______,
    _______, KC_PERC, KC_AMPR, KC_LBRC, KC_RBRC, KC_UNDS, KC_BSLS, G(KC_LEFT),    G(KC_RIGHT), KC_PGUP,       KC_PGDN, _______,
    _______, _______, _______, _______, _______, _______, _______, _______,       _______,     _______,       KC_TRNS, _______
),

/* Upper
 * ,-----------------------------------------------------------------------------------.
 * |      |   1  |   2  |   3  |   4  |   5  |   ^  |   _  |   +  |   *  |   /  |A-Bksp|
 * |------+------+------+------+------+------+------+------+------+------+------+------+
 * |      |   6  |   7  |   8  |   9  |   0  |   <  |   -  |   =  |   (  |   )  |      |
 * |------+------+------+------+------+------+------+------+------+------+------+------+
 * |      |      |      |      |Space |      |   >  |      |   ,  |   .  |Bri-Up|Bri-Dn|
 * |------+------+------+------+------+------+------+------+------+------+------+------+
 * |      |      |      |      |      |Space |      |Raise |      |      |      |      |
 * `-----------------------------------------------------------------------------------'
 */
[_RAISE] = LAYOUT_ortho_4x12(
    _______, KC_1,    KC_2,    KC_3,    KC_4,    KC_5,    KC_CIRC, KC_UNDS, KC_PLUS, KC_ASTR,  KC_SLSH,     A(KC_BSPC),
    _______, KC_6,    KC_7,    KC_8,    KC_9,    KC_0,    KC_LT,   KC_MINS, KC_EQL,  KC_LPRN,  KC_RPRN,     _______,
    _______, _______, _______, _______, KC_SPC,  _______, KC_GT,   _______, KC_COMM, KC_DOT,   KC_BRID,     KC_BRIU,
    _______, _______, _______, _______, _______, KC_SPC,  _______, _______, _______, KC__MUTE, KC__VOLDOWN, KC__VOLUP
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
[_PLOVER] = LAYOUT_planck_grid(
    XXXXXXX, KC_Q,    KC_W,    KC_E,    KC_R,    KC_T,    KC_Y,    KC_U,    KC_I,    KC_O,    KC_P,    KC_LBRC,
    XXXXXXX, KC_A,    KC_S,    KC_D,    KC_F,    KC_G,    KC_H,    KC_J,    KC_K,    KC_L,    KC_SCLN, KC_QUOT,
    XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX, XXXXXXX,
    EXT_PLV, XXXXXXX, XXXXXXX, KC_1,    KC_C,    KC_V,    KC_N,    KC_M,    KC_1,    XXXXXXX, XXXXXXX, XXXXXXX
),

/* Adjust (Lower + Raise)
 *                      v------------------------RGB CONTROL--------------------v
 * ,-----------------------------------------------------------------------------------.
 * |      | Reset|Debug | RGB  |RGBMOD| HUE+ | HUE- | SAT+ | SAT- |BRGTH+|BRGTH-|      |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |      |      |Aud on|Audoff|      |      |Colemk|Qwerty|      |Plover|      |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |      |      |      |      |      |      |      |      |      |      |      |
 * |------+------+------+------+------+------+------+------+------+------+------+------|
 * |      |      |      |      |      |      |      |      |      |      |      |      |
 * `-----------------------------------------------------------------------------------'
 */
[_ADJUST] = LAYOUT_planck_grid(
    _______, RESET,   DEBUG,   RGB_TOG, RGB_MOD, RGB_HUI, RGB_HUD, RGB_SAI, RGB_SAD,  RGB_VAI, RGB_VAD, _______,
    _______, _______, _______, AU_ON,   AU_OFF,  _______, _______, COLEMAK, QWERTY,   _______, PLOVER,  _______,
    _______, _______, _______, _______, _______, _______, _______, _______, _______,  _______, _______, _______,
    _______, _______, _______, _______, _______, _______, _______, _______, _______,  _______, _______, _______
),

[_NAV] = LAYOUT_ortho_4x12(
    _______, G(A(KC_1)), G(A(KC_2)), G(A(KC_3)), G(A(KC_4)), G(A(KC_5)),    G(C(KC_UP)),   G(S(KC_LBRC)), C(KC_UP),   G(S(KC_RBRC)), _______, _______,
    _______, G(A(KC_6)), G(A(KC_7)), G(A(KC_8)), G(A(KC_9)), G(A(KC_0)),    G(C(KC_DOWN)), G(S(KC_TAB)),  G(KC_TAB),  G(KC_GRAVE),   _______, _______,
    _______, _______,    _______,    _______,    KC_CAPS,    G(A(S(KC_V))), G(S(KC_4)),    G(S(C(KC_4))), G(S(KC_3)), G(S(C(KC_3))), _______, _______,
    _______, _______,    _______,    _______,    _______,    _______,       _______,       _______,       _______,    _______,       _______, _______
),

};

#ifdef AUDIO_ENABLE
  float plover_song[][2]     = SONG(PLOVER_SOUND);
  float plover_gb_song[][2]  = SONG(PLOVER_GOODBYE_SOUND);
#endif

layer_state_t layer_state_set_user(layer_state_t state) {
  switch (get_highest_layer(state)) {
    case _RAISE:
        rgblight_setrgb (0x00,  0x00, 0xFF);
        break;
    case _LOWER:
        rgblight_setrgb (0xFF,  0x00, 0x00);
        break;
    case _PLOVER:
        rgblight_setrgb (0x00,  0xFF, 0x00);
        break;
    case _ADJUST:
        rgblight_setrgb (0x7A,  0x00, 0xFF);
        break;
    case _NAV:
        rgblight_setrgb (0x00,  0x7A, 0xFF);
        break;
    case _FUNC:
        rgblight_setrgb (0x00,  0xFF, 0x7A);
        break;
    default: //  for any other layers, or the default layer
        rgblight_setrgb (0x00,  0xFF, 0xFF);
        break;
    }
  return state;
}

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  switch (keycode) {
    case QWERTY:
      if (record->event.pressed) {
        print("mode just switched to qwerty and this is a huge string\n");
        set_single_persistent_default_layer(_QWERTY);
      }
      return false;
      break;
    case COLEMAK:
      if (record->event.pressed) {
        set_single_persistent_default_layer(_COLEMAK);
      }
      return false;
      break;
    case PLOVER:
      if (record->event.pressed) {
        #ifdef AUDIO_ENABLE
          stop_all_notes();
          PLAY_SONG(plover_song);
        #endif
        layer_off(_RAISE);
        layer_off(_LOWER);
        layer_off(_ADJUST);
        layer_on(_PLOVER);
        if (!eeconfig_is_enabled()) {
            eeconfig_init();
        }
        keymap_config.raw = eeconfig_read_keymap();
        keymap_config.nkro = 1;
        eeconfig_update_keymap(keymap_config.raw);
      }
      return false;
      break;
    case EXT_PLV:
      if (record->event.pressed) {
        #ifdef AUDIO_ENABLE
          PLAY_SONG(plover_gb_song);
        #endif
        layer_off(_PLOVER);
      }
      return false;
      break;
  }
  return true;
}

bool muse_mode = false;
