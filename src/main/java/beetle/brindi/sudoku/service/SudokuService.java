package beetle.brindi.sudoku.service;

import beetle.brindi.sudoku.domain.Sudoku;
import beetle.brindi.sudoku.exception.SudokuException;
import beetle.brindi.sudoku.repository.SudokuRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;

import java.util.Optional;
import java.util.Random;

@Service
@RequiredArgsConstructor
public class SudokuService {

    // instantiated by Lombok (RequiredArgsConstructor)
    private final SudokuRepository sudokuRepository;

    public Sudoku getRandomSudoku(){
        Random random = new Random();
        Integer id = random.nextInt(1000000) + 1;
        Optional<Sudoku> optionalSudoku = sudokuRepository.findById(id);

        if ( !optionalSudoku.isPresent() ) {
            throw new SudokuException(HttpStatus.NOT_FOUND, "Sudoku Quiz not found in repository, id: " + id);
        }

        return optionalSudoku.get();
    }

}
