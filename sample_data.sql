BEGIN; 

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

INSERT INTO "defaultbootflags"
        (image_id,key,value)
    VALUES
        (1,'coreos.autologin',NULL);

INSERT INTO "boxen"
        (name,mac,boot_image,boot_until)
    VALUES
        ('haruko','BCAEC5126679',1,now());

INSERT INTO "bootflags"
        (box_id,key,value)
    VALUES
        (1,'key1','value');

INSERT INTO "bootflags"
        (box_id,key,value)
    VALUES
        (1,'key2',NULL);

COMMIT;
