/**
 * @file
 */

#ifndef _SE_LINEBUF_H_
#define _SE_LINEBUF_H_

#include <se_types.h>

#define  LINEBUF_MAX_LINE_LENGTH 20

/**
 * Initialize the buffer.
 *
 * A buffer may be re-initialized.
 *
 * During initialization,
 *    no put operation can be called.
 *    no get operation can be called.
 *
 * ensure: !linebuf_line_available
 * ensure: !linebuf_char_available
 */
void    linebuf_initialize (se_Char len);

/**
 * The maximal length of a line terminated by newline, which
 * can be put into the buffer. A line is a sequence of non-newline
 * and non-carriage-return characters.
 * A newline or carriage return character is not considered a part of a line.
 */
se_Char linebuf_max_length(void);

/**
 * A newline character can always be put.
 * In case of overrun the last character is replaced by a newline character.
 */
void    linebuf_put_char (se_Char byte);

/**
 * put_string uses put_char to place the string into the buffer.
 */
void    linebuf_put_string (const se_Char* str);

/**
 * If overrun no more characters are accepted, until linebuf_initialize is
 * called. All lines and characters in the buffer can be read upto the
 * character, after which overrun occurred.
 */
se_Bool    linebuf_line_overrun   (void);

/**
 * @return Is a newline terminated line available in the buffer.
 */
se_Bool    linebuf_line_available (void);

/**
 * Is the current read position at the end of the line.
 */
se_Bool linebuf_line_at_end (void);

/**
 * Clear the current line which may be under construction.
 */
void    linebuf_line_clear (void);

/**
 * The first character of the current line to be read is returned.
 * The start of current line in the buffer is moved 
 * to the next character.
 *
 * @require linebuf_char_available
 */
se_Char linebuf_get_char (void);

se_Bool linebuf_scan_space(se_Char cnt);
se_Bool linebuf_scan_char(se_Char expected);
se_Bool linebuf_scan_string(se_String expected);
se_Bool linebuf_scan_short(void);
se_Bool linebuf_scan_endline(void);

se_Short  linebuf_last_short(void);
se_Uint16 linebuf_last_uint16(void);

/**
 * Release the current available line.
 *
 * @require linebuf_line_available.
 */
void linebuf_line_release (void);

void linebuf_line_mark (void);
void linebuf_line_reset (void);

#ifdef CUNIT
se_Char* auxtest_linebuf_buffer (void);
se_Int   auxtest_linebuf_buflen (void);
se_Int auxtest_linebuf_xread_start (void);
se_Int auxtest_linebuf_xread_mark (void);
se_Int auxtest_linebuf_xread_pos   (void);
se_Int auxtest_linebuf_xwrite_start (void);
se_Int auxtest_linebuf_xwrite_pos   (void);
#endif
#endif
