#include "dining_philosophers_list.hpp"
#include "philosopher.hpp"

#include <ap/argument_parser.hpp>

#include <cstdint>
#include <thread>
#include <vector>

int main(int argc, char** argv) {
    ap::argument_parser parser;
    parser.program_name("philosophers")
          .program_description("Dining philosophers problem simulation")
          .default_optional_arguments({ap::default_optarg::help});

    parser.add_optional_argument<uint16_t>("philosophers", "p")
          .default_value(static_cast<uint16_t>(5))
          .help("Number of philosophers in the simulation");
    parser.add_optional_argument<uint16_t>("max-meals", "m")
          .default_value(static_cast<uint16_t>(10))
          .help("Number of meals required for one philosopher to be full");

    try {
        parser.parse_args(argc, argv);
    }
    catch (const ap::argument_parser_error& err) {
        std::cerr << "[ERROR] : " << err.what() << "\n" << parser << "\n";
        std::exit(EXIT_FAILURE);
    }

    if (parser.value<bool>("help")) {
        std::cout << parser << "\n";
        std::exit(EXIT_SUCCESS);
    }

    const auto num_philosophers = parser.value<uint16_t>("philosophers");
    const auto max_meals = parser.value<uint16_t>("max-meals");

    std::cout << "Args parsed"
              << "\n\tphilosophers = " << num_philosophers
              << "\n\tmax-meals = " << max_meals
              << "\nStarting execution!\n\n";

    std::vector<std::unique_ptr<std::binary_semaphore>> cutlery_semaphore_list;
    for (uint16_t p_id = 0; p_id < num_philosophers; p_id++)
        cutlery_semaphore_list.push_back(std::make_unique<std::binary_semaphore>(1));

    jpp::dining_philosophers_list dinig_list;

    std::vector<std::thread> philosopher_treads;
    for (uint16_t p_id = 0; p_id < num_philosophers; p_id++) {
        philosopher_treads.emplace_back(
            jpp::philosopher::start_thread,
            p_id, num_philosophers, max_meals,
            std::ref(cutlery_semaphore_list), std::ref(dinig_list)
        );
    }

    for (std::thread& pt : philosopher_treads)
        pt.join();

    std::cout << "\nAll philosophers have finished eating!" << std::endl;

    return 0;
}
