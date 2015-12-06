BEGIN; 

/* Image 1 */

INSERT INTO "images"
        (name,kernel_path,created)
    VALUES
        ('arch','arch/vmlinuz',now());

INSERT INTO "cpios"
        (image_id,ordering,cpio_path)
    VALUES
        (1,0,'arch/initramfs');

INSERT INTO "defaultbootflags"
        (image_id,key,value)
    VALUES
        (1,'user','single');

/* Image 2 */

INSERT INTO "images"
        (name,kernel_path,created)
    VALUES
        ('coreos','coreos/vmlinuz',now());

INSERT INTO "cpios"
        (image_id,ordering,cpio_path)
    VALUES
        (1,0,'coreos/coreos.cpio.gz');

INSERT INTO "defaultbootflags"
        (image_id,key,value)
    VALUES
        (1,'coreos.autologin',NULL);

/* Box 1 */

INSERT INTO "boxen"
        (name,mac,boot_image,boot_until,boot_forever)
    VALUES
        ('haruko','BCAEC5126679',1,now(),TRUE);

INSERT INTO "bootflags"
        (box_id,key,value)
    VALUES
        (1,'key1','value');

INSERT INTO "bootflags"
        (box_id,key,value)
    VALUES
        (1,'key2',NULL);

INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'arch',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'fedora',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.2',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.3',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.4',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.5',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.6',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.7',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.8',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.9',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.10',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.11',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.12',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.13',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.14',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.15',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.16',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.17',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.18',now());
INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (1,'CentOS 6.3.2.19',now());

/* Box 2 */

INSERT INTO "boxen"
        (name,mac,boot_image,boot_until,boot_forever)
    VALUES
        ('toothpaste','BCAEC5126848',1,now(),FALSE);

INSERT INTO "bootflags"
        (box_id,key,value)
    VALUES
        (1,'coreos.autologin',NULL);

INSERT INTO "boots"
        (box_id,image_name,boot_time)
    VALUES
        (2,'Fedora 17',now());

/* Box 3 */

INSERT INTO "boxen"
        (name,mac,boot_image,boot_until,boot_forever)
    VALUES
        ('energia','BCAEC5129222',1,NULL,FALSE);

COMMIT;
