class Loops {
    public static void main(String[] a) {
        new tester().fun();
    }
}


class tester {
    public int fun() {
        int a;

        a=0;
        while (a<10) {
            if (a==0) {
                a=a+2;
                System.out.println(2);
            }
            else if(a<4) {
                while (a<5) {
                    if (a<4) {
                        a=a+1;
                        System.out.println(1);
                    }
                    else {
                        a=a+2;
                        System.out.println(2);
                    }
                }
            }
            else if(a<8) {
                a=a+2;
                System.out.println(2);
            }
            else {
                a=a+1;
                System.out.println(1);
            }
        }
        return 1;
    }
}
