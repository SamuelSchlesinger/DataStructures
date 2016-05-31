import java.util.HashMap;

public class TST<Value> {

    public static void main(String[] args) {
        TST<Integer> tst;
        HashMap<String, Integer> hashmap;
        int N = 2;
        for (int trial = 0; trial < 32; trial++) {
            tst = new TST<Integer>();
            hashmap = new HashMap<String, Integer>();
            N = N * 2;
            System.out.println(N);
            long s = System.currentTimeMillis();
            for (int i = 0; i < N; i++) {
                tst.insert(String.format("%s", i), i);            
            }
            long t = System.currentTimeMillis();
            System.out.printf("TST %d %d\n",N, t - s);
            s = System.currentTimeMillis();
            for (int i = 0; i < N; i++) {
                hashmap.put(String.format("%s", i), i);
            }
            t = System.currentTimeMillis();
            System.out.printf("HASHMAP %d %d\n",N, t - s);
        }
    }
    
    private static final int LEFT_CHILD = 1;
    private static final int RIGHT_CHILD = 2;
    private static final int MIDDLE_CHILD = 3;
    private static final int UNINITIALIZED = -1;

    protected Node head;

    public TST() {
        this.head = null;
    }

    public Value get(String key) {
        if (key == null) {
            throw new java.lang.IllegalArgumentException();
        }
        char[] chars = key.toCharArray();
        int index = 0;
        int N = chars.length;
        Node current = head;
        while (current != null && index != N) {
            if (current.c > chars[index]) {
                current = current.left;
            } else if (current.c < chars[index]) {
                current = current.right;
            } else { 
                index++;
                if (index == N)
                    return current.val;
                current = current.middle;
            }
        }
        return null;
    }

    public void insert(String key, Value val) {
        // not safe to try to insert null valued arguments
        if (key == null || val == null || key.length() == 0) {
            throw new java.lang.IllegalArgumentException();
        }
        // convert string to more amenable representation
        char[] chars = key.toCharArray();
        int N = chars.length;
        int index = 0;
        int childType = UNINITIALIZED;
        // if there's no head, you have to put one there
        if (head == null) {
            if (N == 1) {
                this.head = new Node(chars[0], val);
                return; // if the key is one character long then you're done here
            }
            this.head = new Node(chars[0], null);
            index++;
            insertRest(this.head, index, chars, val);
            return;
        }
        Node parent = head;
        Node current = head;
        // this loop traces the path through as far as it can
        while (current != null && index != N) {
            if (current.c > chars[index]) {
                childType = LEFT_CHILD;
                parent = current;
                current = current.left;
            } else if (current.c < chars[index]) {
                childType = RIGHT_CHILD;
                parent = current;
                current = current.right;
            } else {
                childType = MIDDLE_CHILD;
                parent = current;
                current = current.middle;
	        index++;
            }
        }
        if (index == N) { // true if parent is meant to store val
            parent.val = val;
            return;
        } else { // still more key to tread
            Node fin;
            switch (childType) {
                case LEFT_CHILD:
                    parent.left = new Node(chars[index++], null);
                    if (index == N) { // end of the road
                        parent.left.val = val;
                    } else {            
                        insertRest(parent.left, index, chars, val);    
                    }
                    return;
                case RIGHT_CHILD:
                    parent.right = new Node(chars[index++], null);
                    if (index == N) {
                        parent.right.val = val;
                    } else {
                        insertRest(parent.right, index, chars, val);
                    }
                    return;
                case MIDDLE_CHILD:
                    insertRest(parent, index, chars, val);
                    return;
                case UNINITIALIZED:
                default:
                    System.out.println("IF THIS HAPPENS U MESSED W MY CODE");
            }
        }
    }
 
    /**
    * When you've found a lack of nodes, finish off here
    */
    private void insertRest(Node current, int index, char[] chars, Value val) {
        for (int i = index; i < chars.length; i++) {
            current.middle = new Node(chars[i], null);
            current = current.middle;
        }
        current.val = val;
    }

    /**
    * TST Node subclass
    * If not null, left contains a character which is less than this one.
    * If not null, right contains a character which is greater than this one.
    * If not null, val makes this Node a terminal node storing this 
    * particular value.
    */
    protected class Node {
        protected Node left;
        protected Node middle;
        protected Node right;
        protected Value val;
        protected char c;      

        protected Node(char c, Value val) {
            this.left = null;
            this.middle = null;
            this.right = null;
            this.c = c;
        }

        protected Value value() {
            return val;
        }
    }
}
