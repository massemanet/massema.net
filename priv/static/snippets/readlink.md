I upgraded my trusty Raspberry Pi to raspbian jessie. Alas, a bit of a snag
manifested itself when I realized the ssh server had not started. After some
swearing I realized the "Illegal instruction" error message I saw meant that I
had somehow persuaded apt to install a package compiled for the wrong
architecture. The sshd binary seemed OK;
```
$ readelf -A $(which sshd) | grep Tag_CPU_arch
   Tag_CPU_arch: v6
```
So, a shared library then. This monstrosity did the trick;
```
$ for f in $(ldd /usr/sbin/sshd | cut -f2 -d">" | cut -f1 -d"(") ; do echo $f ; readelf -A $f | grep CPU_arch ; done
...
/lib/arm-linux-gnueabihf/libkeyutils.so.1
  Tag_CPU_arch: v7
...
```
Reinstalling would presumably do the trick. `dpkg` can find the right package;
```
$ dpkg -S libkeyutils.so
libkeyutils1:armhf: /lib/arm-linux-gnueabihf/libkeyutils.so.1
```
Another two hours well spent in Linux-land!
