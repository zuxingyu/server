#ifndef ITEM_UUIDFUNC_INCLUDED
#define ITEM_UUIDFUNC_INCLUDED

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


#include "item.h"

class Item_func_uuid: public Item_str_func
{
public:
  Item_func_uuid(THD *thd): Item_str_func(thd) {}
  bool fix_length_and_dec()
  {
    collation.set(DTCollation_numeric());
    fix_char_length(MY_UUID_STRING_LENGTH);
    return FALSE;
  }
  bool const_item() const { return false; }
  table_map used_tables() const { return RAND_TABLE_BIT; }
  const char *func_name() const{ return "uuid"; }
  String *val_str(String *);
  bool check_vcol_func_processor(void *arg)
  {
    return mark_unsupported_function(func_name(), "()", arg, VCOL_NON_DETERMINISTIC);
  }
  Item *get_copy(THD *thd)
  { return get_item_copy<Item_func_uuid>(thd, this); }
};

#endif // ITEM_UUIDFUNC_INCLUDED
