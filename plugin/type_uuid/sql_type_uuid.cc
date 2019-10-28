/*
   Copyright (c) 2019 MariaDB Corporation

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; version 2 of the License.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1335  USA */

#define MYSQL_SERVER
#include "mariadb.h"
#include "my_net.h"
#include "sql_class.h" // THD, SORT_FIELD_ATTR
#include "opt_range.h" // SEL_ARG
#include "sql_type_uuid.h"


static bool get_digit(char ch, uint *val)
{
  if (ch >= '0' && ch <= '9')
  {
    *val= (uint) ch - '0';
    return false;
  }
  if (ch >= 'a' && ch <= 'f')
  {
    *val= (uint) ch - 'a' + 0x0a;
    return false;
  }
  if (ch >= 'A' && ch <= 'F')
  {
    *val= (uint) ch - 'A' + 0x0a;
    return false;
  }
  return true;
}


bool UUID::ascii_to_uuid(const char *str, size_t str_length)
{
  if (str_length < 32 || str_length > 3 * binary_length() - 1)
    return true;

  uint oidx= 0;
  for (const char *s= str; s < str + str_length; )
  {
    if (oidx >= binary_length())
      goto err;
    if (*s == '-')
    {
      if (s == str)
        goto err;
      s++;
      continue;
    }
    uint hi, lo;
    if (get_digit(*s++, &hi) || get_digit(*s++, &lo))
      goto err;
    m_buffer[oidx++]= (char) ((hi << 4) + lo);
  }
  return false;
err:
  bzero(m_buffer, sizeof(m_buffer));
  return true;
}


size_t UUID::to_string(char *dst, size_t dstsize) const
{
  my_uuid2str((const uchar *) m_buffer, dst);
  return MY_UUID_STRING_LENGTH;
}


bool UUID::make_from_item(Item *item)
{
  if (item->type_handler() == &type_handler_uuid)
  {
    Native tmp(m_buffer, sizeof(m_buffer));
    bool rc= item->val_native(current_thd, &tmp);
    if (rc)
      return true;
    DBUG_ASSERT(tmp.length() == sizeof(m_buffer));
    if (tmp.ptr() != m_buffer)
      memcpy(m_buffer, tmp.ptr(), sizeof(m_buffer));
    return false;
  }
  StringBufferUUID tmp;
  String *str= item->val_str(&tmp);
  return str ? make_from_character_or_binary_string(str) : true;
}


bool UUID::make_from_character_or_binary_string(const String *str)
{
  static Name name= type_handler_uuid.name();
  if (str->charset() != &my_charset_bin)
  {
    bool rc= character_string_to_uuid(str->ptr(), str->length(),
                                      str->charset());
    if (rc)
      current_thd->push_warning_wrong_value(Sql_condition::WARN_LEVEL_WARN,
                                            name.ptr(),
                                            ErrConvString(str).ptr());
    return rc;
  }
  if (str->length() != sizeof(m_buffer))
  {
    current_thd->push_warning_wrong_value(Sql_condition::WARN_LEVEL_WARN,
                                          name.ptr(),
                                          ErrConvString(str).ptr());
    return true;
  }
  DBUG_ASSERT(str->ptr() != m_buffer);
  memcpy(m_buffer, str->ptr(), sizeof(m_buffer));
  return false;
};


/********************************************************************/


class cmp_item_uuid: public cmp_item_scalar
{
  UUID m_native;
public:
  cmp_item_uuid()
   :cmp_item_scalar(),
    m_native(UUID_zero())
  { }
  void store_value(Item *item) override
  {
    m_native= UUID(item, &m_null_value);
  }
  int cmp_not_null(const Value *val) override
  {
    DBUG_ASSERT(!val->is_null());
    DBUG_ASSERT(val->is_string());
    UUID_null tmp(val->m_string);
    DBUG_ASSERT(!tmp.is_null());
    return m_native.cmp(tmp);
  }
  int cmp(Item *arg) override
  {
    UUID_null tmp(arg);
    return m_null_value || tmp.is_null() ? UNKNOWN : m_native.cmp(tmp) != 0;
  }
  int compare(cmp_item *ci) override
  {
    cmp_item_uuid *tmp= static_cast<cmp_item_uuid*>(ci);
    DBUG_ASSERT(!m_null_value);
    DBUG_ASSERT(!tmp->m_null_value);
    return m_native.cmp(tmp->m_native);
  }
  cmp_item *make_same() override
  {
    return new cmp_item_uuid();
  }
};


class Field_uuid: public Field
{
  static void set_min_value(char *ptr)
  {
    memset(ptr, 0, UUID::binary_length());
  }
  static void set_max_value(char *ptr)
  {
    memset(ptr, 0xFF, UUID::binary_length());
  }
  void store_warning(const ErrConv &str,
                     Sql_condition::enum_warning_level level)
  {
    static const Name type_name= type_handler_uuid.name();
    if (get_thd()->count_cuted_fields > CHECK_FIELD_EXPRESSION)
      get_thd()->push_warning_truncated_value_for_field(level, type_name.ptr(),
                                                        str.ptr(), table->s,
                                                        field_name.str);
  }
  int set_null_with_warn(const ErrConv &str)
  {
    store_warning(str, Sql_condition::WARN_LEVEL_WARN);
    set_null();
    return 1;
  }
  int set_min_value_with_warn(const ErrConv &str)
  {
    store_warning(str, Sql_condition::WARN_LEVEL_WARN);
    set_min_value((char*) ptr);
    return 1;
  }
  int set_max_value_with_warn(const ErrConv &str)
  {
    store_warning(str, Sql_condition::WARN_LEVEL_WARN);
    set_max_value((char*) ptr);
    return 1;
  }
  int store_uuid_null_with_warn(const UUID_null &uuid,
                                const ErrConvString &err)
  {
    DBUG_ASSERT(marked_for_write_or_computed());
    if (uuid.is_null())
      return maybe_null() ? set_null_with_warn(err) :
                            set_min_value_with_warn(err);
    uuid.to_binary((char *) ptr, UUID::binary_length());
    return 0;
  }

public:
  Field_uuid(const LEX_CSTRING *field_name_arg, const Record_addr &rec)
    :Field(rec.ptr(), UUID::max_char_length(),
           rec.null_ptr(), rec.null_bit(), Field::NONE, field_name_arg)
  {
    flags|= BINARY_FLAG | UNSIGNED_FLAG;
  }
  const Type_handler *type_handler() const override
  {
    return &type_handler_uuid;
  }
  uint32 max_display_length() const override { return field_length; }
  bool str_needs_quotes() const override { return true; }
  const DTCollation &dtcollation() const override
  {
    static DTCollation_numeric c;
    return c;
  }
  CHARSET_INFO *charset(void) const override { return &my_charset_numeric; }
  const CHARSET_INFO *sort_charset(void) const override { return &my_charset_bin; }
  /**
    This makes client-server protocol convert the value according
    to @@character_set_client.
  */
  bool binary() const override { return false; }
  enum ha_base_keytype key_type() const override { return HA_KEYTYPE_BINARY; }

  bool is_equal(const Column_definition &new_field) const override
  {
    return new_field.type_handler() == type_handler();
  }
  bool eq_def(const Field *field) const override
  {
    return Field::eq_def(field);
  }
  double pos_in_interval(Field *min, Field *max) override
  {
    return pos_in_interval_val_str(min, max, 0);
  }
  int cmp(const uchar *a, const uchar *b) const override
  { return memcmp(a, b, pack_length()); }

  void sort_string(uchar *to, uint length) override
  {
    DBUG_ASSERT(length == pack_length());
    memcpy(to, ptr, length);
  }
  uint32 pack_length() const override
  {
    return UUID::binary_length();
  }
  uint pack_length_from_metadata(uint field_metadata) const override
  {
    return UUID::binary_length();
  }

  void sql_type(String &str) const override
  {
    static Name name= type_handler_uuid.name();
    str.set_ascii(name.ptr(), name.length());
  }

  bool validate_value_in_record(THD *thd, const uchar *record) const override
  {
    return false;
  }

  String *val_str(String *val_buffer,
                  String *val_ptr __attribute__((unused))) override
  {
    DBUG_ASSERT(marked_for_read());
    UUID_null tmp((const char *) ptr, pack_length());
    return tmp.to_string(val_buffer) ? NULL : val_buffer;
  }

  my_decimal *val_decimal(my_decimal *to) override
  {
    DBUG_ASSERT(marked_for_read());
    my_decimal_set_zero(to);
    return to;
  }

  longlong val_int() override
  {
    DBUG_ASSERT(marked_for_read());
    return 0;
  }

  double val_real() override
  {
    DBUG_ASSERT(marked_for_read());
    return 0;
  }

  bool get_date(MYSQL_TIME *ltime, date_mode_t fuzzydate) override
  {
    DBUG_ASSERT(marked_for_read());
    set_zero_time(ltime, MYSQL_TIMESTAMP_TIME);
    return false;
  }

  bool val_bool(void) override
  {
    DBUG_ASSERT(marked_for_read());
    return !UUID::only_zero_bytes((const char *) ptr, UUID::binary_length());
  }

  int store_native(const Native &value) override
  {
    DBUG_ASSERT(marked_for_write_or_computed());
    DBUG_ASSERT(value.length() == UUID::binary_length());
    memcpy(ptr, value.ptr(), value.length());
    return 0;
  }

  int store(const char *str, size_t length, CHARSET_INFO *cs) override
  {
    return cs == &my_charset_bin ? store_binary(str, length) :
                                   store_text(str, length, cs);
  }

  int store_text(const char *str, size_t length, CHARSET_INFO *cs) override
  {
    return store_uuid_null_with_warn(UUID_null(str, length, cs),
                                     ErrConvString(str, length, cs));
  }

  int store_binary(const char *str, size_t length) override
  {
    return store_uuid_null_with_warn(UUID_null(str, length),
                                     ErrConvString(str, length,
                                                   &my_charset_bin));
  }

  int store_hex_hybrid(const char *str, size_t length) override
  {
    return Field_uuid::store_binary(str, length);
  }

  int store_decimal(const my_decimal *num) override
  {
    DBUG_ASSERT(marked_for_write_or_computed());
    return set_min_value_with_warn(ErrConvDecimal(num));
  }

  int store(longlong nr, bool unsigned_flag) override
  {
    DBUG_ASSERT(marked_for_write_or_computed());
    return set_min_value_with_warn(
            ErrConvInteger(Longlong_hybrid(nr, unsigned_flag)));
  }

  int store(double nr) override
  {
    DBUG_ASSERT(marked_for_write_or_computed());
    return set_min_value_with_warn(ErrConvDouble(nr));
  }

  int store_time_dec(const MYSQL_TIME *ltime, uint dec) override
  {
    DBUG_ASSERT(marked_for_write_or_computed());
    return set_min_value_with_warn(ErrConvTime(ltime));
  }

  /*** Field conversion routines ***/
  int store_field(Field *from) override
  {
    // INSERT INTO t1 (uuid_field) SELECT different_field_type FROM t2;
    return from->save_in_field(this);
  }
  int save_in_field(Field *to) override
  {
    // INSERT INTO t2 (different_field_type) SELECT uuid_field FROM t1;
    if (to->charset() == &my_charset_bin &&
        dynamic_cast<const Type_handler_general_purpose_string*>
          (to->type_handler()))
    {
      NativeBufferUUID res;
      val_native(&res);
      return to->store(res.ptr(), res.length(), &my_charset_bin);
    }
    return save_in_field_str(to);
  }
  Copy_func *get_copy_func(const Field *from) const override
  {
    // ALTER to UUID from another field
    return do_field_string;
  }

  Copy_func *get_copy_func_to(const Field *to) const override
  {
    if (type_handler() == to->type_handler())
    {
      // ALTER from UUID to UUID
      DBUG_ASSERT(pack_length() == to->pack_length());
      DBUG_ASSERT(charset() == to->charset());
      DBUG_ASSERT(sort_charset() == to->sort_charset());
      return Field::do_field_eq;
    }
    // ALTER from UUID to another data type
    if (to->charset() == &my_charset_bin &&
        dynamic_cast<const Type_handler_general_purpose_string*>
          (to->type_handler()))
    {
      /*
        ALTER from UUID to a binary string type, e.g.:
          BINARY, TINYBLOB, BLOB, MEDIUMBLOB, LONGBLOB
      */
      return do_field_uuid_native_to_binary;
    }
    return do_field_string;
  }

  static void do_field_uuid_native_to_binary(Copy_field *copy)
  {
    NativeBufferUUID res;
    copy->from_field->val_native(&res);
    copy->to_field->store(res.ptr(), res.length(), &my_charset_bin);
  }

  bool memcpy_field_possible(const Field *from) const override
  {
    // INSERT INTO t1 (uuid_field) SELECT field2 FROM t2;
    return type_handler() == from->type_handler();
  }
  enum_conv_type rpl_conv_type_from(const Conv_source &source,
                                    const Relay_log_info *rli,
                                    const Conv_param &param) const override
  {
    if (type_handler() == source.type_handler() ||
        (source.type_handler() == &type_handler_string &&
         source.type_handler()->max_display_length_for_field(source) ==
         UUID::binary_length()))
      return rpl_conv_type_from_same_data_type(source.metadata(), rli, param);
    return CONV_TYPE_IMPOSSIBLE;
  }

  /*** Optimizer routines ***/
  bool test_if_equality_guarantees_uniqueness(const Item *const_item) const override
  {
    /*
      This condition:
        WHERE uuid_field=const
      should return a single distinct value only,
      as comparison is done according to UUID.
    */
    return true;
  }
  bool can_be_substituted_to_equal_item(const Context &ctx,
                                        const Item_equal *item_equal)
                                        override
  {
    switch (ctx.subst_constraint()) {
    case ANY_SUBST:
      return ctx.compare_type_handler() == item_equal->compare_type_handler();
    case IDENTITY_SUBST:
      return true;
    }
    return false;
  }
  Item *get_equal_const_item(THD *thd, const Context &ctx,
                             Item *const_item) override;
  bool can_optimize_keypart_ref(const Item_bool_func *cond,
                                const Item *item) const override
  {
    DBUG_ASSERT(item->type_handler()->is_traditional_scalar_type() ||
                item->type_handler() == type_handler());
    return true;
  }
  /**
    Test if Field can use range optimizer for a standard comparison operation:
      <=, <, =, <=>, >, >=
    Note, this method does not cover spatial operations.
  */
  bool can_optimize_range(const Item_bool_func *cond,
                          const Item *item,
                          bool is_eq_func) const override
  {
    // See the DBUG_ASSERT comment in can_optimize_keypart_ref()
    DBUG_ASSERT(item->type_handler()->is_traditional_scalar_type() ||
                item->type_handler() == type_handler());
    return true;
  }
  SEL_ARG *get_mm_leaf(RANGE_OPT_PARAM *prm, KEY_PART *key_part,
                       const Item_bool_func *cond,
                       scalar_comparison_op op, Item *value) override
  {
    DBUG_ENTER("Field_uuid::get_mm_leaf");
    if (!can_optimize_scalar_range(prm, key_part, cond, op, value))
      DBUG_RETURN(0);
    int err= value->save_in_field_no_warnings(this, 1);
    if ((op != SCALAR_CMP_EQUAL && is_real_null()) || err < 0)
      DBUG_RETURN(&null_element);
    if (err > 0)
    {
      if (op == SCALAR_CMP_EQ || op == SCALAR_CMP_EQUAL)
        DBUG_RETURN(new (prm->mem_root) SEL_ARG_IMPOSSIBLE(this));
      DBUG_RETURN(NULL); /*  Cannot infer anything */
    }
    DBUG_RETURN(stored_field_make_mm_leaf(prm, key_part, op, value));
  }
  bool can_optimize_hash_join(const Item_bool_func *cond,
                                      const Item *item) const override
  {
    return can_optimize_keypart_ref(cond, item);
  }
  bool can_optimize_group_min_max(const Item_bool_func *cond,
                                  const Item *const_item) const override
  {
    return true;
  }

  uint row_pack_length() const override { return pack_length(); }

  Binlog_type_info binlog_type_info() const override
  {
    DBUG_ASSERT(type() == binlog_type());
    return Binlog_type_info_fixed_string(Field_uuid::binlog_type(),
                                         UUID::binary_length(),
                                         &my_charset_bin);
  }

  uchar *pack(uchar *to, const uchar *from, uint max_length) override
  {
    DBUG_PRINT("debug", ("Packing field '%s'", field_name.str));
    return StringPack(&my_charset_bin, UUID::binary_length()).
             pack(to, from, max_length);
  }

  const uchar *unpack(uchar *to, const uchar *from, const uchar *from_end,
                      uint param_data) override
  {
    return StringPack(&my_charset_bin, UUID::binary_length()).
             unpack(to, from, from_end, param_data);
  }

  uint max_packed_col_length(uint max_length)
  {
    return StringPack::max_packed_col_length(max_length);
  }

  uint packed_col_length(const uchar *data_ptr, uint length)
  {
    return StringPack::packed_col_length(data_ptr, length);
  }

  /**********/
  uint size_of() const override { return sizeof(*this); }
};


class Item_typecast_uuid: public Item_func
{
public:
  Item_typecast_uuid(THD *thd, Item *a) :Item_func(thd, a) {}

  const Type_handler *type_handler() const override
  { return &type_handler_uuid; }

  enum Functype functype() const override { return CHAR_TYPECAST_FUNC; }
  bool eq(const Item *item, bool binary_cmp) const override
  {
    if (this == item)
      return true;
    if (item->type() != FUNC_ITEM ||
        functype() != ((Item_func*)item)->functype())
      return false;
    if (type_handler() != item->type_handler())
      return false;
    Item_typecast_uuid *cast= (Item_typecast_uuid*) item;
    return args[0]->eq(cast->args[0], binary_cmp);
  }
  const char *func_name() const override { return "cast_as_uuid"; }
  void print(String *str, enum_query_type query_type) override
  {
    str->append(STRING_WITH_LEN("cast("));
    args[0]->print(str, query_type);
    str->append(STRING_WITH_LEN(" as uuid)"));
  }
  bool fix_length_and_dec() override
  {
    Type_std_attributes::operator=(Type_std_attributes_uuid());
    return false;
  }
  String *val_str(String *to) override
  {
    UUID_null tmp(args[0]);
    return (null_value= tmp.is_null() || tmp.to_string(to)) ? NULL : to;
  }
  longlong val_int() override
  {
    return 0;
  }
  double val_real() override
  {
    return 0;
  }
  my_decimal *val_decimal(my_decimal *to) override
  {
    my_decimal_set_zero(to);
    return to;
  }
  bool get_date(THD *thd, MYSQL_TIME *ltime, date_mode_t fuzzydate) override
  {
    set_zero_time(ltime, MYSQL_TIMESTAMP_TIME);
    return false;
  }
  bool val_native(THD *thd, Native *to) override
  {
    UUID_null tmp(args[0]);
    return null_value= tmp.is_null() || tmp.to_native(to);
  }
  Item *get_copy(THD *thd) override
  { return get_item_copy<Item_typecast_uuid>(thd, this); }
};


class Item_cache_uuid: public Item_cache
{
  NativeBufferUUID m_value;
public:
  Item_cache_uuid(THD *thd)
   :Item_cache(thd, &type_handler_uuid)
  { }
  Item *get_copy(THD *thd)
  { return get_item_copy<Item_cache_uuid>(thd, this); }
  bool cache_value()
  {
    if (!example)
      return false;
    value_cached= true;
    null_value= example->val_native_with_conversion_result(current_thd,
                                                           &m_value,
                                                           type_handler());
    return true;
  }
  String* val_str(String *to)
  {
    if (!has_value())
      return NULL;
    UUID_null tmp(m_value.ptr(), m_value.length());
    return tmp.is_null() || tmp.to_string(to) ? NULL : to;
  }
  my_decimal *val_decimal(my_decimal *to)
  {
    if (!has_value())
      return NULL;
    my_decimal_set_zero(to);
    return to;
  }
  longlong val_int()
  {
    if (!has_value())
      return 0;
    return 0;
  }
  double val_real()
  {
    if (!has_value())
      return 0;
    return 0;
  }
  longlong val_datetime_packed(THD *thd)
  {
    DBUG_ASSERT(0);
    if (!has_value())
      return 0;
    return 0;
  }
  longlong val_time_packed(THD *thd)
  {
    DBUG_ASSERT(0);
    if (!has_value())
      return 0;
    return 0;
  }
  bool get_date(THD *thd, MYSQL_TIME *ltime, date_mode_t fuzzydate)
  {
    if (!has_value())
      return true;
    set_zero_time(ltime, MYSQL_TIMESTAMP_TIME);
    return false;
  }
  bool val_native(THD *thd, Native *to)
  {
    if (!has_value())
      return true;
    return to->copy(m_value.ptr(), m_value.length());
  }
};


class Item_literal_uuid: public Item_literal
{
  UUID m_value;
public:
  Item_literal_uuid(THD *thd)
   :Item_literal(thd),
    m_value(UUID_zero())
  { }
  Item_literal_uuid(THD *thd, const UUID &value)
   :Item_literal(thd),
    m_value(value)
  { }
  const Type_handler *type_handler() const override
  {
    return &type_handler_uuid;
  }
  longlong val_int() override
  {
    return 0;
  }
  double val_real() override
  {
    return 0;
  }
  String *val_str(String *to) override
  {
    return m_value.to_string(to) ? NULL : to;
  }
  my_decimal *val_decimal(my_decimal *to) override
  {
    my_decimal_set_zero(to);
    return to;
  }
  bool get_date(THD *thd, MYSQL_TIME *ltime, date_mode_t fuzzydate) override
  {
    set_zero_time(ltime, MYSQL_TIMESTAMP_TIME);
    return false;
  }
  bool val_native(THD *thd, Native *to) override
  {
    return m_value.to_native(to);
  }
  void print(String *str, enum_query_type query_type) override
  {
    StringBufferUUID tmp;
    m_value.to_string(&tmp);
    str->append("UUID'");
    str->append(tmp);
    str->append('\'');
  }
  Item *get_copy(THD *thd) override
  { return get_item_copy<Item_literal_uuid>(thd, this); }

  // Non-overriding methods
  void set_value(const UUID &value)
  {
    m_value= value;
  }
};


class in_uuid :public in_vector
{
  UUID m_value;
  static int cmp_uuid(void *cmp_arg, UUID *a, UUID *b)
  {
    return a->cmp(*b);
  }
public:
  in_uuid(THD *thd, uint elements)
   :in_vector(thd, elements, sizeof(UUID), (qsort2_cmp) cmp_uuid, 0),
    m_value(UUID_zero())
  { }
  const Type_handler *type_handler() const override
  {
    return &type_handler_uuid;
  }
  void set(uint pos, Item *item) override
  {
    UUID *buff= &((UUID *) base)[pos];
    UUID_null value(item);
    if (value.is_null())
      *buff= UUID_zero();
    else
      *buff= value;
  }
  uchar *get_value(Item *item) override
  {
    UUID_null value(item);
    if (value.is_null())
      return 0;
    m_value= value;
    return (uchar *) &m_value;
  }
  Item* create_item(THD *thd) override
  {
    return new (thd->mem_root) Item_literal_uuid(thd);
  }
  void value_to_item(uint pos, Item *item) override
  {
    const UUID &buff= (((UUID*) base)[pos]);
    static_cast<Item_literal_uuid*>(item)->set_value(buff);
  }
};


class Item_char_typecast_func_handler_uuid_to_binary:
                                       public Item_handled_func::Handler_str
{
public:
  const Type_handler *return_type_handler(const Item_handled_func *item)
                                          const override
  {
    if (item->max_length > MAX_FIELD_VARCHARLENGTH)
      return Type_handler::blob_type_handler(item->max_length);
    if (item->max_length > 255)
      return &type_handler_varchar;
    return &type_handler_string;
  }
  bool fix_length_and_dec(Item_handled_func *xitem) const override
  {
    return false;
  }
  String *val_str(Item_handled_func *item, String *to) const override
  {
    DBUG_ASSERT(dynamic_cast<const Item_char_typecast*>(item));
    return static_cast<Item_char_typecast*>(item)->
             val_str_binary_from_native(to);
  }
};


static Item_char_typecast_func_handler_uuid_to_binary
         item_char_typecast_func_handler_uuid_to_binary;


bool Type_handler_uuid::
  Item_char_typecast_fix_length_and_dec(Item_char_typecast *item) const
{
  if (item->cast_charset() == &my_charset_bin)
  {
    item->fix_length_and_dec_native_to_binary(UUID::binary_length());
    item->set_func_handler(&item_char_typecast_func_handler_uuid_to_binary);
    return false;
  }
  item->fix_length_and_dec_str();
  return false;
}


bool
Type_handler_uuid::character_or_binary_string_to_native(THD *thd,
                                                         const String *str,
                                                         Native *to) const
{
  if (str->charset() == &my_charset_bin)
  {
    // Convert from a binary string
    if (str->length() != UUID::binary_length() ||
        to->copy(str->ptr(), str->length()))
    {
      thd->push_warning_wrong_value(Sql_condition::WARN_LEVEL_WARN,
                                    name().ptr(),
                                    ErrConvString(str).ptr());
      return true;
    }
    return false;
  }
  // Convert from a character string
  UUID_null tmp(*str);
  if (tmp.is_null())
    thd->push_warning_wrong_value(Sql_condition::WARN_LEVEL_WARN,
                                  name().ptr(),
                                  ErrConvString(str).ptr());
  return tmp.is_null() || tmp.to_native(to);
}


bool
Type_handler_uuid::Item_save_in_value(THD *thd,
                                       Item *item,
                                       st_value *value) const
{
  value->m_type= DYN_COL_STRING;
  String *str= item->val_str(&value->m_string);
  if (str != &value->m_string && !item->null_value)
  {
    // "item" returned a non-NULL value
    if (UUID_null(*str).is_null())
    {
      // The value was not-null, but conversion to UUID failed.
      thd->push_warning_wrong_value(Sql_condition::WARN_LEVEL_WARN,
                                    name().ptr(),
                                    ErrConvString(str).ptr());
      value->m_type= DYN_COL_NULL;
      return true;
    }
    // "item" returned a non-NULL value, and it was a valid UUID
    value->m_string.set(str->ptr(), str->length(), str->charset());
  }
  return check_null(item, value);
}


void Type_handler_uuid::Item_param_setup_conversion(THD *thd,
                                                     Item_param *param) const
{
  param->setup_conversion_string(thd, thd->variables.character_set_client);
}


void Type_handler_uuid::make_sort_key(uchar *to, Item *item,
                                       const SORT_FIELD_ATTR *sort_field,
                                       Sort_param *param) const
{
  DBUG_ASSERT(item->type_handler() == this);
  NativeBufferUUID tmp;
  item->val_native_result(current_thd, &tmp);
  if (item->maybe_null)
  {
    if (item->null_value)
    {
      memset(to, 0, UUID::binary_length() + 1);
      return;
    }
    *to++= 1;
  }
  DBUG_ASSERT(!item->null_value);
  DBUG_ASSERT(UUID::binary_length() == tmp.length());
  DBUG_ASSERT(UUID::binary_length() == sort_field->length);
  memcpy(to, tmp.ptr(), tmp.length());
}


void Type_handler_uuid::sortlength(THD *thd,
                                    const Type_std_attributes *item,
                                    SORT_FIELD_ATTR *attr) const
{
  attr->length= UUID::binary_length();
  attr->suffix_length= 0;
}


cmp_item *Type_handler_uuid::make_cmp_item(THD *thd, CHARSET_INFO *cs) const
{
  return new (thd->mem_root) cmp_item_uuid;
}



in_vector *
Type_handler_uuid::make_in_vector(THD *thd, const Item_func_in *func,
                                   uint nargs) const
{
  return new (thd->mem_root) in_uuid(thd, nargs);
}


Item *Type_handler_uuid::create_typecast_item(THD *thd, Item *item,
                                               const Type_cast_attributes &attr)
                                               const
{
  return new (thd->mem_root) Item_typecast_uuid(thd, item);
}


Item_cache *Type_handler_uuid::Item_get_cache(THD *thd, const Item *item) const
{
  return new (thd->mem_root) Item_cache_uuid(thd);
}


Item *
Type_handler_uuid::make_const_item_for_comparison(THD *thd,
                                                   Item *src,
                                                   const Item *cmp) const
{
  UUID_null tmp(src);
  if (tmp.is_null())
    return new (thd->mem_root) Item_null(thd, src->name.str);
  return new (thd->mem_root) Item_literal_uuid(thd, tmp);
}


Item *Field_uuid::get_equal_const_item(THD *thd, const Context &ctx,
                                        Item *const_item)
{
  UUID_null tmp(const_item);
  if (tmp.is_null())
    return NULL;
  return new (thd->mem_root) Item_literal_uuid(thd, tmp);
}


Field *
Type_handler_uuid::make_table_field_from_def(
                                     TABLE_SHARE *share,
                                     MEM_ROOT *mem_root,
                                     const LEX_CSTRING *name,
                                     const Record_addr &addr,
                                     const Bit_addr &bit,
                                     const Column_definition_attributes *attr,
                                     uint32 flags) const
{
  return new (mem_root) Field_uuid(name, addr);
}


Field *Type_handler_uuid::make_table_field(MEM_ROOT *root,
                                            const LEX_CSTRING *name,
                                            const Record_addr &addr,
                                            const Type_all_attributes &attr,
                                            TABLE_SHARE *share) const
{
  return new (root) Field_uuid(name, addr);
}


Field *Type_handler_uuid::make_conversion_table_field(MEM_ROOT *root,
                                                       TABLE *table,
                                                       uint metadata,
                                                       const Field *target)
                                                       const
{
  const Record_addr tmp(NULL, Bit_addr(true));
  return new (table->in_use->mem_root) Field_uuid(&empty_clex_str, tmp);
}


bool Type_handler_uuid::partition_field_check(const LEX_CSTRING &field_name,
                                               Item *item_expr) const
{
  if (item_expr->cmp_type() != STRING_RESULT)
  {
    my_error(ER_WRONG_TYPE_COLUMN_VALUE_ERROR, MYF(0));
    return true;
  }
  return false;
}


bool
Type_handler_uuid::partition_field_append_value(
                                          String *to,
                                          Item *item_expr,
                                          CHARSET_INFO *field_cs,
                                          partition_value_print_mode_t mode)
                                          const
{
  StringBufferUUID uuidstr;
  UUID_null uuid(item_expr);
  if (uuid.is_null())
  {
    my_error(ER_PARTITION_FUNCTION_IS_NOT_ALLOWED, MYF(0));
    return true;
  }
  return uuid.to_string(&uuidstr) ||
         to->append('\'') ||
         to->append(uuidstr) ||
         to->append('\'');
}


/***************************************************************/


class Type_collection_uuid: public Type_collection
{
  const Type_handler *aggregate_common(const Type_handler *a,
                                       const Type_handler *b) const
  {
    if (a == b)
      return a;
    return NULL;
  }
  const Type_handler *aggregate_if_string(const Type_handler *a,
                                          const Type_handler *b) const
  {
    static const Type_aggregator::Pair agg[]=
    {
      {&type_handler_uuid, &type_handler_null,        &type_handler_uuid},
      {&type_handler_uuid, &type_handler_varchar,     &type_handler_uuid},
      {&type_handler_uuid, &type_handler_string,      &type_handler_uuid},
      {&type_handler_uuid, &type_handler_tiny_blob,   &type_handler_uuid},
      {&type_handler_uuid, &type_handler_blob,        &type_handler_uuid},
      {&type_handler_uuid, &type_handler_medium_blob, &type_handler_uuid},
      {&type_handler_uuid, &type_handler_long_blob,   &type_handler_uuid},
      {&type_handler_uuid, &type_handler_hex_hybrid,  &type_handler_uuid},
      {NULL,NULL,NULL}
    };
    return Type_aggregator::find_handler_in_array(agg, a, b, true);
  }
public:
  const Type_handler *aggregate_for_result(const Type_handler *a,
                                           const Type_handler *b)
                                           const override
  {
    const Type_handler *h;
    if ((h= aggregate_common(a, b)) ||
        (h= aggregate_if_string(a, b)))
      return h;
    return NULL;
  }

  const Type_handler *aggregate_for_min_max(const Type_handler *a,
                                            const Type_handler *b)
                                            const override
  {
    return aggregate_for_result(a, b);
  }

  const Type_handler *aggregate_for_comparison(const Type_handler *a,
                                               const Type_handler *b)
                                               const override
  {
    if (const Type_handler *h= aggregate_common(a, b))
      return h;
    static const Type_aggregator::Pair agg[]=
    {
      {&type_handler_uuid, &type_handler_null,      &type_handler_uuid},
      {&type_handler_uuid, &type_handler_long_blob, &type_handler_uuid},
      {NULL,NULL,NULL}
    };
    return Type_aggregator::find_handler_in_array(agg, a, b, true);
  }

  const Type_handler *aggregate_for_num_op(const Type_handler *a,
                                           const Type_handler *b)
                                           const override
  {
    return NULL;
  }

  const Type_handler *handler_by_name(const LEX_CSTRING &name) const override
  {
    if (type_handler_uuid.name().eq(name))
      return &type_handler_uuid;
    return NULL;
  }
};


const Type_collection *Type_handler_uuid::type_collection() const
{
  static Type_collection_uuid type_collection_uuid;
  return &type_collection_uuid;
}
