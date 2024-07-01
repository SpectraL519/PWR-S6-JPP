#include "philosopher.hpp"

#include "dining_philosophers_list.hpp"

#include <chrono>
#include <iostream>

#define RSEED 42

#define MAX_RAND_TIME 1000
#define EATING_TIME_OFFSET 200
#define THINKING_TIME_OFFSET 500

namespace jpp {

philosopher::philosopher(const uint16_t id, const uint16_t num_philosophers)
: _id(id), _left_cutlery_id(id), _right_cutlery_id((id + 1) % num_philosophers) {}

uint16_t philosopher::id() const {
    return this->_id;
}

std::string philosopher::to_string() const {
    return "(C" + std::to_string(this->_left_cutlery_id)
         + "-P" + std::to_string(this->_id)
         + "-C" + std::to_string(this->_right_cutlery_id)
         + ":M" + std::to_string(this->_meals_eaten) + ")";
    // TODO: use std::format
}

void philosopher::_start(
    const uint16_t max_meals,
    std::vector<std::unique_ptr<std::binary_semaphore>>& cutlery_semaphore_list,
    dining_philosophers_list& dinig_list
) {
    static std::mt19937 gen32;
    static std::uniform_int_distribution<uint16_t> uniform{1, MAX_RAND_TIME - 1};

    while (true) {
        // think
        std::this_thread::sleep_for(
            std::chrono::milliseconds(uniform(gen32) + THINKING_TIME_OFFSET)
        );

        // acquire cutlery
        cutlery_semaphore_list[this->_left_cutlery_id]->acquire();
        cutlery_semaphore_list[this->_right_cutlery_id]->acquire();

        // add to dining list
        this->_meals_eaten++;
        dinig_list.add_philosopher(*this);

        // eat
        std::this_thread::sleep_for(
            std::chrono::milliseconds(uniform(gen32) + EATING_TIME_OFFSET)
        );

        // remove from dining list
        dinig_list.remove_philosopher(this->_id);

        // release cutlery
        cutlery_semaphore_list[this->_left_cutlery_id]->release();
        cutlery_semaphore_list[this->_right_cutlery_id]->release();


        if (bool finished_eating = this->_meals_eaten == max_meals; finished_eating) {
            std::cout << "[Finished eating: id = " << this->_id << "]\n";
            return;
        }
    }
}

void philosopher::start_thread(
    const uint16_t p_id,
    const uint16_t num_philosophers,
    const uint16_t max_meals,
    std::vector<std::unique_ptr<std::binary_semaphore>>& cutlery_semaphore_list,
    dining_philosophers_list& dinig_list
) {
    philosopher p{p_id, num_philosophers};
    p._start(max_meals, cutlery_semaphore_list, dinig_list);
}

} // namespace jpp
