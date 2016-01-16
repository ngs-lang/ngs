{print}

# Track rule name for error messages
/RULE.*yy_([a-zA-Z0-9_]+)/ {
	match($0, /yy_([a-zA-Z0-9_]+)/, r); f=r[1];
}
/YY_LOCAL.*yymatchChar/ {
	f="(a character)";
}
/YY_LOCAL.*yymatchClass/ {
	f="(a class)";
}
/YY_LOCAL.*yymatchString/ {
	f="(a string)";
}
/yyprintf.*fail/ {
	print "  HANDLE_FAILED_MATCH(\"" f "\");"
}

# Location tracking
/action= action/ {
	print "  memcpy(yy->__thunks[yy->__thunkpos].location, location, sizeof(yy->__thunks[yy->__thunkpos].location));"

}
