class StackArgs {
    public static void main(String[] a) {
        System.out.println(new SA().go(1, 1, 2, 3, 5, 8, 13, 21, 34));
    }
}

class SA {
    public int go(int a, int b, int c, int d, int e, int f, int g, int h, int i) {
        System.out.println(a);
        System.out.println(b);
        System.out.println(c);
        System.out.println(d);
        System.out.println(e);
        System.out.println(f);
        System.out.println(g);
        System.out.println(h);
        System.out.println(i);

        return 0;
    }
}
