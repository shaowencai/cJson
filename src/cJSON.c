/* cJSON */
/* JSON parser in C. */

#include <string.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <float.h>
#include <limits.h>
#include <ctype.h>
#include "cJSON.h"

static const char *parse_value(cJSON *item,const char *value,char *buf,int bufSize,int *pidex);

static int cJSON_strcasecmp(const char *s1,const char *s2)
{
	if (!s1) return (s1==s2)?0:1;if (!s2) return 1;
	for(; tolower(*s1) == tolower(*s2); ++s1, ++s2)	if(*s1 == 0)	return 0;
	return tolower(*(const unsigned char *)s1) - tolower(*(const unsigned char *)s2);
}

/* 跳过字符串中cr/lf或者空格*/
static const char *skip(const char *in) 
{
	while (in && *in && (unsigned char)*in<=32) in++; 
	return in;
}


static cJSON *cJSON_New_Item(char *buf,int bufSize,int *pidex)
{
	cJSON* node = (cJSON*)&buf[*pidex];
	
	*pidex = *pidex + sizeof(cJSON) + (8 - (sizeof(cJSON)%8));
	
	if( *pidex > bufSize )
	{
		return 0;
	}
	
	memset(node,0,sizeof(cJSON));
	
	return node;
}


/* 将字符串解析为数字 */
static const char *parse_number(cJSON *item,const char *num)
{
	double n=0,sign=1,scale=0;
	int subscale=0,signsubscale=1;

	if (*num=='-') sign=-1,num++;	/* Has sign? */
	if (*num=='0') num++;			/* is zero */
	if (*num>='1' && *num<='9')	do	n=(n*10.0)+(*num++ -'0');	while (*num>='0' && *num<='9');	/* Number? */
	if (*num=='.' && num[1]>='0' && num[1]<='9') {num++;		do	n=(n*10.0)+(*num++ -'0'),scale--; while (*num>='0' && *num<='9');}	/* Fractional part? */
	if (*num=='e' || *num=='E')		/* Exponent? */
	{	num++;if (*num=='+') num++;	else if (*num=='-') signsubscale=-1,num++;	/* With sign? */
		while (*num>='0' && *num<='9') subscale=(subscale*10)+(*num++ - '0');	/* Number? */
	}

	n=sign*n*pow(10.0,(scale+subscale*signsubscale));	/* number = +/- number.fraction * 10^+/- exponent */

	item->valuedouble=n;
	item->valueint=(int)n;
	item->valuelong = (long long)n;
	item->type=cJSON_Number;
	return num;
}

static int pow2gt (int x)	{	--x;	x|=x>>1;	x|=x>>2;	x|=x>>4;	x|=x>>8;	x|=x>>16;	return x+1;	}

static unsigned parse_hex4(const char *str)
{
	unsigned h=0;
	if (*str>='0' && *str<='9') h+=(*str)-'0'; else if (*str>='A' && *str<='F') h+=10+(*str)-'A'; else if (*str>='a' && *str<='f') h+=10+(*str)-'a'; else return 0;
	h=h<<4;str++;
	if (*str>='0' && *str<='9') h+=(*str)-'0'; else if (*str>='A' && *str<='F') h+=10+(*str)-'A'; else if (*str>='a' && *str<='f') h+=10+(*str)-'a'; else return 0;
	h=h<<4;str++;
	if (*str>='0' && *str<='9') h+=(*str)-'0'; else if (*str>='A' && *str<='F') h+=10+(*str)-'A'; else if (*str>='a' && *str<='f') h+=10+(*str)-'a'; else return 0;
	h=h<<4;str++;
	if (*str>='0' && *str<='9') h+=(*str)-'0'; else if (*str>='A' && *str<='F') h+=10+(*str)-'A'; else if (*str>='a' && *str<='f') h+=10+(*str)-'a'; else return 0;
	return h;
}

/* Parse the input text into an unescaped cstring, and populate item. */
static const unsigned char firstByteMark[7] = { 0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };

static const char *parse_string(cJSON *item,const char *str,char *buf,int bufSize,int *pidex)
{
	const char *ptr=str+1;
	char *ptr2;
	char *out;
	int len=0;
	unsigned uc,uc2;
	
	if (*str!='\"') 
	{
		return 0;//不是字符串返回
	}
	
	while (*ptr!='\"' && *ptr && ++len) if (*ptr++ == '\\') ptr++;	/* 跳过引号. */
	
	out= &buf[*pidex];
	
	*pidex = *pidex + (len + 1) +(8 - ((len + 1)%8));
	
	if(*pidex > bufSize)return 0;
	
	ptr=str+1;
	
	ptr2=out;
	
	while (*ptr!='\"' && *ptr)
	{
		if (*ptr!='\\') *ptr2++=*ptr++;
		else
		{
			ptr++;
			switch (*ptr)
			{
				case 'b': *ptr2++='\b';	break;
				case 'f': *ptr2++='\f';	break;
				case 'n': *ptr2++='\n';	break;
				case 'r': *ptr2++='\r';	break;
				case 't': *ptr2++='\t';	break;
				case 'u':	 /*转换utf16为utf8 */
					uc=parse_hex4(ptr+1);
					ptr+=4;

					if ((uc>=0xDC00 && uc<=0xDFFF) || uc==0)	break;

					if (uc>=0xD800 && uc<=0xDBFF)
					{
						if (ptr[1]!='\\' || ptr[2]!='u')	break;	/* missing second-half of surrogate.	*/
						uc2=parse_hex4(ptr+3);ptr+=6;
						if (uc2<0xDC00 || uc2>0xDFFF)		break;	/* invalid second-half of surrogate.	*/
						uc=0x10000 + (((uc&0x3FF)<<10) | (uc2&0x3FF));
					}

					len=4;if (uc<0x80) len=1;else if (uc<0x800) len=2;else if (uc<0x10000) len=3; ptr2+=len;
					
					switch (len) {
						case 4: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
						case 3: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
						case 2: *--ptr2 =((uc | 0x80) & 0xBF); uc >>= 6;
						case 1: *--ptr2 =(uc | firstByteMark[len]);
					}
					ptr2+=len;
					break;
				default:  *ptr2++=*ptr; break;
			}
			ptr++;
		}
	}
	*ptr2=0;
	if (*ptr=='\"') ptr++;
	item->valuestring=out;
	item->type=cJSON_String;
	return ptr;
}


static const char *parse_array(cJSON *item,const char *value,char *buf,int bufSize,int *pidex)
{
	cJSON *child;
	
	if (*value!='[')	
	{
		return 0;/*不是一个数组! */
	}	

	item->type=cJSON_Array;
	
	value=skip(value+1);
	
	if (*value==']') return value+1;	/* 空数组 */

	item->child=child=cJSON_New_Item(buf,bufSize,pidex);
	
	if (!item->child) return 0;		 
	
	value=skip(parse_value(child,skip(value),buf,bufSize,pidex));	
	
	if (!value) return 0;

	while (*value==',')
	{
		cJSON *new_item;
		
		if (!(new_item=cJSON_New_Item(buf,bufSize,pidex))) return 0;
		
		child->next=new_item;
		
		new_item->prev=child;child=new_item;
		
		value=skip(parse_value(child,skip(value+1),buf,bufSize,pidex));
		
		if (!value) return 0;
	}

	if (*value==']') return value+1;	/* end of array */
	
	return 0;	/* malformed. */
}

/* 从字符串中构建对象结构体 */
static const char *parse_object(cJSON *item,const char *value,char *buf,int bufSize,int *pidex)
{
	cJSON *child;
		
	if (*value!='{')	
	{
		return 0;	/*不是一个对象  退出*/
	}
	
	item->type=cJSON_Object;
	
	value=skip(value+1);
	
	if (*value=='}') 
	{
		return value+1;
	}
	
	item->child=child=cJSON_New_Item(buf,bufSize,pidex);
	
	if (!item->child) return 0;
	
	value=skip(parse_string(child,skip(value),buf,bufSize,pidex));
	
	if (!value) return 0;
	
	child->string=child->valuestring;
	
	child->valuestring=0;
	
	if (*value!=':') 
	{
		return 0;
	}
	value=skip(parse_value(child,skip(value+1),buf,bufSize,pidex));	/* 跳过空格. 获取到值*/
	
	if (!value) return 0;

	while (*value==',')
	{
		cJSON *new_item; 
		
		if (!(new_item=cJSON_New_Item(buf,bufSize,pidex)))	return 0;

		child->next=new_item;
		new_item->prev=child;
		child=new_item;
		value=skip(parse_string(child,skip(value+1),buf,bufSize,pidex));
		if (!value) return 0;
		child->string=child->valuestring;
		child->valuestring=0;
		if (*value!=':') 
		{
			return 0;
		}
		value=skip(parse_value(child,skip(value+1),buf,bufSize,pidex));

		if (!value) return 0;
	}
	
	if (*value=='}') return value+1;
	return 0;
}

/* 通过匹配进行实际的解析*/
static const char *parse_value(cJSON *item,const char *value,char *buf,int bufSize,int *pidex)
{
	if (!value)						return 0;	/* Fail on null. */
	if (!strncmp(value,"null",4))	{ item->type=cJSON_NULL;  return value+4; }
	if (!strncmp(value,"false",5))	{ item->type=cJSON_False; return value+5; }
	if (!strncmp(value,"true",4))	{ item->type=cJSON_True; item->valueint=1;	return value+4; }
	if (*value=='\"')				{ return parse_string(item,value,buf,bufSize,pidex); }
	if (*value=='-' || (*value>='0' && *value<='9'))	{ return parse_number(item,value); }
	if (*value=='[')				{ return parse_array(item,value,buf,bufSize,pidex); }
	if (*value=='{')				{ return parse_object(item,value,buf,bufSize,pidex); }

	return 0;	/* 失败返回0*/
}

/*将字符串解析为CJSON对象 */
static cJSON *cJSON_ParseWithOpts(const char *value,char *buf,int bufSize)
{
	const char *end=0;
	int mallocIdex = 0;
	
	cJSON *c=cJSON_New_Item(buf,bufSize,&mallocIdex);
	
	if (!c) return 0;       /* 内存申请失败 */

	end=parse_value(c,skip(value),buf,bufSize,&mallocIdex);

	if (!end)
	{
		return 0;
	}

	return c;
}

/* 供外部调用的接口，传入字符串生成CJSON结构体 */
cJSON *cJSON_Parse(const char *value,char *buf,int bufSize) 
{
	return cJSON_ParseWithOpts(value,buf,bufSize);
}

/* 供外部调用的接口，获取数组的大小  */
int cJSON_GetArraySize(cJSON *array)							
{
	cJSON *c=array->child;
	int i=0;
	while(c)i++,c=c->next;
	return i;
}

/* 供外部调用的接口，通过数组下标得到指定的数组元素  */
cJSON *cJSON_GetArrayItem(cJSON *array,int item)				
{
	if(!array)return 0;
	cJSON *c=array->child;  
	while (c && item>0) item--,c=c->next; 
	return c;
}

/* 供外部调用的接口，通过Json的key值得到对应的键值对 */
cJSON *cJSON_GetObjectItem(cJSON *object,const char *string)	
{
	if(!object)return 0;
	cJSON *c=object->child; 
	while (c && cJSON_strcasecmp(c->string,string)) c=c->next; 
	return c;
}

