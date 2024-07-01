#pragma once

#include <cstdint>
#include <memory>
#include <random>
#include <semaphore>
#include <string>
#include <vector>

namespace jpp {

class dining_philosophers_list;

class philosopher {
public:
    philosopher() = delete;

    philosopher(const philosopher&) = delete;
    philosopher& operator=(const philosopher&) = delete;

    philosopher(const uint16_t id, const uint16_t num_philosophers = 1);

    ~philosopher() = default;

    [[nodiscard]] uint16_t id() const;
    [[nodiscard]] std::string to_string() const;

    static void start_thread(
        const uint16_t p_id,
        const uint16_t num_philosophers,
        const uint16_t max_meals,
        std::vector<std::unique_ptr<std::binary_semaphore>>& cutlery_semaphore_list,
        dining_philosophers_list& dinig_list
    );

private:
    void _start(
        const uint16_t max_meals,
        std::vector<std::unique_ptr<std::binary_semaphore>>& cutlery_semaphore_list,
        dining_philosophers_list& dinig_list
    );

    uint16_t _id;
    uint16_t _left_cutlery_id;
    uint16_t _right_cutlery_id;

    uint16_t _meals_eaten = 0;
};

} // namespace jpp
