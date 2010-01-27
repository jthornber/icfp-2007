#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <math.h>
#include <string.h>

#include "list.h"

/*
 * logging
 */
static void info(char const *str)
{
        //fprintf(stderr, "%s\n", str);
}

static void error(char const *str)
{
        fprintf(stderr, "%s\n", str);
}

static void barf(char const *str)
{
        error(str);
        exit(1);
}

/*
 * Pixel
 */
struct pixel {
        unsigned char red, green, blue, alpha;
};

/*
 * Bucket functions
 */
static unsigned colour_count = 0;
static unsigned bucket_red = 0;
static unsigned bucket_green = 0;
static unsigned bucket_blue = 0;

static unsigned alpha_count = 0;
static unsigned bucket_alpha = 0;

void empty_bucket() {
        colour_count = bucket_red = bucket_green = bucket_blue = alpha_count = bucket_alpha = 0;
}

unsigned current_red() {
        if (colour_count == 0)
                return 0;
        else
                return bucket_red / colour_count;
}

unsigned current_blue() {
        if (colour_count == 0)
                return 0;
        else
                return bucket_blue / colour_count;
}

unsigned current_green() {
        if (colour_count == 0)
                return 0;
        else
                return bucket_green / colour_count;
}

unsigned current_alpha() {
        if (alpha_count == 0)
                return 255;
        else
                return bucket_alpha / alpha_count;
}

void current_pixel(struct pixel *p) {
        unsigned char r = current_red();
        unsigned char g = current_green();
        unsigned char b = current_blue();
        unsigned char a = current_alpha();
        p->red = floor(r * a / 255);
        p->green = floor(g * a / 255);
        p->blue = floor(b * a / 255);
        p->alpha = a;
}

void add_colour(unsigned red, unsigned green, unsigned blue) {
        bucket_red += red;
        bucket_green += green;
        bucket_blue += blue;
        colour_count++;
}

void add_alpha(unsigned alpha) {
        bucket_alpha += alpha;
        alpha_count++;
}

/*
 * Position
 */
struct position {
        unsigned x, y;
};

static struct position current_pos = {0, 0};
static struct position mark = {0, 0};

enum {NORTH, SOUTH, EAST, WEST};

static int current_dir = EAST;

static void inc_pos(unsigned dx, unsigned dy) {
        int nx = (int) current_pos.x + dx;
        int ny = (int) current_pos.y + dy;

        while (nx < 0)
                nx += 600;

        while (nx >= 600)
                nx -= 600;

        while (ny < 0)
                ny += 600;

        while (ny >= 600)
                ny -= 600;

        current_pos.x = (unsigned) nx;
        current_pos.y = (unsigned) ny;
}

static void move_dir(int dir) {
        switch (dir) {
        case NORTH:
                inc_pos(0, -1);
                break;

        case SOUTH:
                inc_pos(0, 1);
                break;

        case EAST:
                inc_pos(1, 0);
                break;

        case WEST:
                inc_pos(-1, 0);
                break;

        default:
                error("unknown direction");
        }
}

static void rotate_clockwise() {
        switch (current_dir) {
        case EAST:
                current_dir = SOUTH;
                break;

        case SOUTH:
                current_dir = WEST;
                break;

        case WEST:
                current_dir = NORTH;
                break;

        case NORTH:
                current_dir = EAST;
                break;

        default:
                error("unexpected direction");
        }
}

static void rotate_anti_clockwise() {
        switch (current_dir) {
        case EAST:
                current_dir = NORTH;
                break;

        case NORTH:
                current_dir = WEST;
                break;

        case WEST:
                current_dir = SOUTH;
                break;

        case SOUTH:
                current_dir = EAST;
                break;

        default:
                error("unexpected direction");
        }
}

/*
 * Bitmaps
 */
struct bitmap {
        struct bitmap *next;
        struct pixel pixels[600 * 600];
};

static unsigned bitmap_count = 0;
struct bitmap *bitmaps = NULL;
struct bitmap *old_bitmaps = NULL;

static struct bitmap *current_bitmap() {
        return bitmaps;
}

static void push_bitmap() {
        if (bitmap_count >= 10)
                return;

        struct bitmap *bm = malloc(sizeof(*bm));
        memset(bm, 0, sizeof(*bm));
        bm->next = bitmaps;
        bitmaps = bm;
        bitmap_count++;
}

static void pop_bitmap() {
        if (bitmap_count < 2)
                return;

        struct bitmap *bm = bitmaps;
        bitmaps = bitmaps->next;
        bitmap_count--;

        bm->next = old_bitmaps;
        old_bitmaps = bm;
}

static void free_bitmaps(struct bitmap *bm) {
        struct bitmap *next;

        while (bm) {
                next = bm->next;
                free(bm);
                bm = next;
        }
}

static unsigned ind(unsigned x, unsigned y) {
        return (y * 600) + x;
}

static struct pixel *get_pixel_b(struct bitmap *bm, unsigned x, unsigned y) {
        return bm->pixels + ind(x, y);
}

static struct pixel *get_pixel(unsigned x, unsigned y) {
        return get_pixel_b(current_bitmap(), x, y);
}

static void set_pixel(unsigned x, unsigned y) {
        current_pixel(current_bitmap()->pixels + ind(x, y));
}

static int max(int a, int b) {
        if (a > b)
                return a;
        else
                return b;
}

static void line(struct position *p1, struct position *p2) {
        unsigned i;
        unsigned dx = p2->x - p1->x;
        unsigned dy = p2->y - p1->y;

        unsigned d = max(abs(dx), abs(dy));
        unsigned c = 0;

        unsigned x, y;

        if (dx * dy <= 0)
                c = 1;

        x = p1->x * d + floor((d - c) / 2);
        y = p1->y * d + floor((d - c) / 2);

        for (i = 0; i < d; i++) {
                set_pixel(floor(x / d), floor(y / d));
                x = x + dx;
                y = y + dy;
        }

        set_pixel(p2->x, p2->y);
}

struct fill_entry {
        struct list_head list;
        struct position pos;
};

static void fill_push(struct list_head *q, unsigned x, unsigned y) {
        struct fill_entry *fe = malloc(sizeof(*fe));
        list_init(&fe->list);
        fe->pos.x = x;
        fe->pos.y = y;
        list_add_tail(&fe->list, q);
}

static int fill_pop(struct list_head *q, unsigned *x, unsigned *y) {
        struct fill_entry *fe = list_entry(q->next, struct fill_entry, list);

        if (list_empty(q))
                return 0;

        *x = fe->pos.x;
        *y = fe->pos.y;

        list_del(&fe->list);
        free(fe);
        return 1;
}

static void fill(struct position *start) {
        struct list_head queue;
        unsigned x, y;
        struct pixel initial = *get_pixel(start->x, start->y);

        list_init(&queue);
        fill_push(&queue, start->x, start->y);

        while (fill_pop(&queue, &x, &y)) {
                struct pixel *px = get_pixel(x, y);
                if (px->alpha != initial.alpha ||
                    px->red != initial.red ||
                    px->green != initial.green ||
                    px->blue != initial.blue)
                        continue;

                set_pixel(x, y);

                if (x > 0)
                        fill_push(&queue, x - 1, y);

                if (x < 599)
                        fill_push(&queue, x + 1, y);

                if (y > 0)
                        fill_push(&queue, x, y - 1);

                if (y < 599)
                        fill_push(&queue, x, y + 1);
        }
}

static void compose() {
        unsigned x, y;
        struct bitmap *b0, *b1;

        if (bitmap_count < 2)
                return;

        b0 = bitmaps;
        b1 = bitmaps->next;

        for (x = 0; x < 599; x++) {
                for (y = 0; y < 599; y++) {
                        struct pixel *p0 = get_pixel_b(b0, x, y);
                        struct pixel *p1 = get_pixel_b(b1, x, y);

                        p1->red = p0->red + floor(p1->red * (255 - p0->alpha) / 255);
                        p1->green = p0->green + floor(p1->green * (255 - p0->alpha) / 255);
                        p1->blue = p0->blue + floor(p1->blue * (255 - p0->alpha) / 255);
                        p1->alpha = p0->alpha + floor(p1->alpha * (255 - p0->alpha) / 255);
                }
        }

        pop_bitmap();
}

static void clip() {
        unsigned x, y;
        struct bitmap *b0, *b1;

        if (bitmap_count < 2)
                return;

        b0 = bitmaps;
        b1 = bitmaps->next;

        for (x = 0; x < 599; x++) {
                for (y = 0; y < 599; y++) {
                        struct pixel *p0 = get_pixel_b(b0, x, y);
                        struct pixel *p1 = get_pixel_b(b1, x, y);

                        p1->red = floor(p1->red * p0->alpha / 255);
                        p1->green = floor(p1->green * p0->alpha / 255);
                        p1->blue = floor(p1->blue * p0->alpha / 255);
                        p1->alpha = floor(p0->alpha * p1->alpha / 255);
                }
        }

        pop_bitmap();
}

/*
 * writing raw graphics files
 */
static void write_raw(struct bitmap *bm) {
        char linebuf[600 * 3];
        FILE *file;
        int x, y;

        file = fopen("out.raw", "wb");
        if (file == NULL)
                error("couldn't open output file");

        for (y = 0; y < 600; y++) {
                char *ptr = linebuf;
                for (x = 0; x < 600; x++) {
                        struct pixel *p = get_pixel(x, y);
                        *ptr++ = (char) p->red;
                        *ptr++ = (char) p->green;
                        *ptr++ = (char) p->blue;
                }
                fwrite(linebuf, 1, sizeof(linebuf), file);
        }

        fclose(file);
}

/*
 * Top level
 */
int main(int argc, char **argv)
{
        int fd, r;
        unsigned char *ptr, *end;
        struct stat buf;

        if (argc != 2)
                barf("Usage: draw <bytecode file>");

        fd = open(argv[1], O_RDONLY);
        if (fd < 0)
                barf("open failed");

        r = fstat(fd, &buf);
        if (r < 0)
                barf("stat failed");

        ptr = mmap(NULL, buf.st_size, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0);
        if (ptr == MAP_FAILED)
                barf("mmap failed");

        /* we need at least one bitmap */
        push_bitmap();

        end = ptr + buf.st_size;
        while (ptr != end) {
                switch (*ptr) {
                case 0:
                        info("BLACK");
                        add_colour(0, 0, 0);
                        break;

                case 1:
                        info("RED");
                        add_colour(255, 0, 0);
                        break;

                case 2:
                        info("GREEN");
                        add_colour(0, 255, 0);
                        break;

                case 3:
                        info("YELLOW");
                        add_colour(255, 255, 0);
                        break;

                case 4:
                        info("BLUE");
                        add_colour(0, 0, 255);
                        break;

                case 5:
                        info("MAGENTA");
                        add_colour(255, 0, 255);
                        break;

                case 6:
                        info("CYAN");
                        add_colour(0, 255, 255);
                        break;

                case 7:
                        info("WHITE");
                        add_colour(255, 255, 255);
                        break;

                case 8:
                        info("TRANSPARENT");
                        add_alpha(0);
                        break;

                case 9:
                        info("OPAQUE");
                        add_alpha(255);
                        break;

                case 10:
                        info("EMPTY_BUCKET");
                        empty_bucket();
                        break;

                case 11:
                        info("MOVE");
                        move_dir(current_dir);
                        break;

                case 12:
                        info("TURN_ANTI_CLOCKWISE");
                        rotate_anti_clockwise();
                        break;

                case 13:
                        info("TURN_CLOCKWISE");
                        rotate_clockwise();
                        break;

                case 14:
                        info("MARK");
                        mark = current_pos;
                        break;

                case 15:
                        info("LINE");
                        line(&current_pos, &mark);
                        break;

                case 16:
                        info("FILL");
                        {
                                struct pixel new;
                                struct pixel *old = get_pixel(current_pos.x, current_pos.y);

                                current_pixel(&new);

                                if (new.alpha != old->alpha ||
                                    new.red != old->red ||
                                    new.green != old->green ||
                                    new.blue != old->blue)
                                        fill(&current_pos);
                        }
                        break;

                case 17:
                        info("ADD_BITMAP");
                        push_bitmap();
                        break;

                case 18:
                        info("COMPOSE");
                        compose();
                        break;

                case 19:
                        info("CLIP");
                        clip();
                        break;

                case 20:
                        info("NOOP");
                        break;

                default:
                        barf("unexpected instruction");
                }

                ptr++;
        }

        write_raw(bitmaps);

        free_bitmaps(bitmaps);
        free_bitmaps(old_bitmaps);

        return 0;
}
