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
#include "item_uuidfunc.h"
#include "sql_type_uuid.h"

String *Item_func_uuid::val_str(String *str)
{
  DBUG_ASSERT(fixed == 1);
  if (!UUID_generated().to_string(str))
    return str;
  str->set("", 0, collation.collation);
  return str;
}
