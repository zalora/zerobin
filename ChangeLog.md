1.5.1
=====

* Added ChangeLog
* zerobin command supports reading stdin:
	`zerobin -f /etc/fstab` can be done as
	`cat /etc/fstab | zerobin -f -` or `zerobin -f < /etc/fstab`
