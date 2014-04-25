class Call {
    public static void main(String[] a) {
        System.out.println(new A().go());
    }
}

class A {
    public int go() {
        return this.go2();
    }

    public int go2() {
        return 1337;
    }
}
