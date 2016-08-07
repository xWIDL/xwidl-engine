// ===== Traits ======
trait Bar {

}
trait Foo {
    method use_bar(bar: Bar) 
        requires bar != null
        
}
// ===== TopLevelMethods =====
method Main(foo: Foo)
    requires foo != null {
    foo.use_bar(null);
}
