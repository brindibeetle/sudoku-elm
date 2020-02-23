package beetle.brindi.sudoku.exception;

import lombok.AllArgsConstructor;
import lombok.Data;
import org.springframework.http.HttpStatus;

@Data
@AllArgsConstructor
public class SudokuException extends RuntimeException {

    private final HttpStatus status;

    private final String message;

}
