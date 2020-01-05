package thinkinjava;

import java.lang.annotation.*;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

public class DBAnnotation {
    public static void main(String[] args){
        Class<?> clz = UserTable.class;

        DBTable dbTable = clz.getAnnotation(DBTable.class);
        if (dbTable == null) {
            throw new RuntimeException("No DBTable annotation in class!");
        }
        String tableName = dbTable.value();

        List<String> columnDefs = new ArrayList<>();
        for(Field field : clz.getDeclaredFields()){
            Annotation[] annotations = field.getDeclaredAnnotations();
            if(annotations.length < 1) continue;
            if(annotations[0] instanceof SQLString){
                SQLString sqlString = (SQLString)annotations[0];
                columnDefs.add(sqlString.value() + " VARCHAR(" + sqlString.size() + ")" + getConstraints(sqlString.constraints()));
            }else if(annotations[0] instanceof SQLInt){
                SQLInt sqlInt = (SQLInt)annotations[0];
                columnDefs.add(sqlInt.value()+ " INT " + getConstraints(sqlInt.constraints()));
            }
        }
        String sql = "CREATE TABLE " + tableName + "(\n  " + String.join(",\n  ", columnDefs) + "\n);";

        System.out.println(sql);
        /**
         * CREATE TABLE user(
         *   uid INT  PRIMARY KEY  NOT NULL,
         *   name VARCHAR(10)
         * );
         */
    }
    public static String getConstraints(Constraints constraints){
        String res = "";
        if(constraints.primaryKey()){
            res += " PRIMARY KEY ";
        }
        if(constraints.unique()){
            res += " UNIQUE ";
        }
        if(!constraints.allowNull()){
            res += " NOT NULL";
        }
        return res;
    }
}

@DBTable("user")
class UserTable{
    @SQLInt(value = "uid", constraints = @Constraints(primaryKey = true))
    public Integer uid;
    @SQLString("name")
    public String name;
}

/**
 * 用于标识类为数据库并指定数据库名
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@interface DBTable{
    String value();
}

/**
 * 约束条件
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@interface Constraints{
    boolean primaryKey() default false;
    boolean allowNull() default false;
    boolean unique() default false;
}

/**
 * 字符串字段
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@interface SQLString{
    String value();
    int size() default 10;
    Constraints constraints() default @Constraints(allowNull = true);
}
/**
 * 整形字段
 */
@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
@interface SQLInt{
    String value();
    Constraints constraints() default @Constraints(allowNull = true);
}
