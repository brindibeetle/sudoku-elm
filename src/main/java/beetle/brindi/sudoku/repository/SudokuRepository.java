package beetle.brindi.sudoku.repository;

import beetle.brindi.sudoku.domain.Sudoku;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface SudokuRepository extends MongoRepository<Sudoku, Integer> {

//    Sudoku findById(Integer id);
}
