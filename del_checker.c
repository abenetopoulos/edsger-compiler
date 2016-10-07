#include <stdint.h>
#include <stdlib.h>

//int64_t new(int16_t);
void* new(int16_t);

typedef struct {
    uint64_t startAddress;
    uint64_t size;    
} allocation;

typedef struct ln_t {
    allocation addr;
    struct ln_t* next;
} list_node;


list_node *allocatedList = NULL;

void allocate(int16_t size) {
    void* newPointer = new(size);
    list_node *node = (list_node *) malloc(sizeof(list_node));

    node->addr.startAddress = (uint64_t) newPointer;
    node->addr.size = size;
    node->next = NULL;

    if (allocatedList == NULL) {
        allocatedList = node;
    }else{
        list_node *current = allocatedList;
        while (current->next != NULL) {
            current = current->next;
        }

        current->next = node;
    }
}

void delete(int64_t addr) {
    if (allocatedList == NULL)
        return;
    else {
        if (allocatedList->addr.startAddress < addr && addr < allocatedList->addr.startAddress + allocatedList->addr.size) {
            list_node *tmp = allocatedList;
            allocatedList = allocatedList->next;
            free(tmp);
            return;
        }

        list_node *current = allocatedList;
        while (current->next != NULL) {
            if (current->next->addr.startAddress < addr && addr < current->next->addr.startAddress + current->next->addr.size) {
                list_node* tmp = current->next;
                current->next = tmp->next;
                free(tmp);
                return;
            }

            current = current->next;
        }
    }
}
