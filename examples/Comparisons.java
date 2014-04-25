class Call {
    public static void main(String[] a) {
        System.out.println(new A().go());
    }
}

class A {
    public int go() {
        int i;

        if (3 < 4 || 4 > 3 || 3 <= 4 || 4 >= 3 || 4 == 4 || 4 != 4 || !false && true) {
            i = 1337;
        } else {
            i = 0;
        }

        return 1337;
    }
}
