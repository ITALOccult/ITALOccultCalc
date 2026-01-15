using System.Text;

namespace Occult.Asteroids
{
	internal class BinaryAsteroidOffsets
	{
		private double[] offset_RA_mas = new double[4];

		private double[] offset_Dec_mas = new double[4];

		private double[] offset_JD = new double[4];

		private double sigmaMajor_mas;

		private double sigmaMinor_mas;

		private double sigmaPAMajor = 90.0;

		private double errorEllipseOffsetRA_mas;

		private double errorEllipseOffsetDec_dec;

		private double errorX_mas;

		private double errorY_mas;

		private string solution = "";

		private string solutionID = "";

		private double diameter = 10.0;

		private double diameterUncertainty = 1.0;

		private string name;

		private int componentSeqNum;

		public double[] Offset_RA_mas
		{
			get
			{
				return offset_RA_mas;
			}
			set
			{
				offset_RA_mas = value;
			}
		}

		public double[] Offset_Dec_mas
		{
			get
			{
				return offset_Dec_mas;
			}
			set
			{
				offset_Dec_mas = value;
			}
		}

		public double[] Offset_JD
		{
			get
			{
				return offset_JD;
			}
			set
			{
				offset_JD = value;
			}
		}

		public double ComponentDiameter
		{
			get
			{
				return diameter;
			}
			set
			{
				diameter = value;
			}
		}

		public double ComponentDiameterUncertainty
		{
			get
			{
				return diameterUncertainty;
			}
			set
			{
				diameterUncertainty = value;
			}
		}

		public string ComponentName
		{
			get
			{
				return name;
			}
			set
			{
				name = value;
			}
		}

		public int ComponentSeqNum
		{
			get
			{
				return componentSeqNum;
			}
			set
			{
				componentSeqNum = value;
			}
		}

		public string SolutionType
		{
			get
			{
				return solution;
			}
			set
			{
				solution = value;
			}
		}

		public string SolutionID
		{
			get
			{
				return solutionID;
			}
			set
			{
				solutionID = value;
			}
		}

		public double SigmaMajor_mas
		{
			get
			{
				return sigmaMajor_mas;
			}
			set
			{
				sigmaMajor_mas = value;
			}
		}

		public double SigmaMinor_mas
		{
			get
			{
				return sigmaMinor_mas;
			}
			set
			{
				sigmaMinor_mas = value;
			}
		}

		public double SigmaPAMajor_deg
		{
			get
			{
				return sigmaPAMajor;
			}
			set
			{
				sigmaPAMajor = value;
			}
		}

		public double ErrorEllipseOffsetRA_mas
		{
			get
			{
				return errorEllipseOffsetRA_mas;
			}
			set
			{
				errorEllipseOffsetRA_mas = value;
			}
		}

		public double ErrorEllipseOffsetDec_mas
		{
			get
			{
				return errorEllipseOffsetDec_dec;
			}
			set
			{
				errorEllipseOffsetDec_dec = value;
			}
		}

		public double UncertaintyInX_mas
		{
			get
			{
				return errorX_mas;
			}
			set
			{
				errorX_mas = value;
			}
		}

		public double UncertaintyInX_asec => errorX_mas / 1000.0;

		public double UncertaintyInY_mas
		{
			get
			{
				return errorY_mas;
			}
			set
			{
				errorY_mas = value;
			}
		}

		public double UncertaintyInY_asec => errorY_mas / 1000.0;

		public override string ToString()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendLine(ComponentName + string.Format("  Diameter = {0,1:f1}km   Solution ", ComponentDiameter) + SolutionID + " : " + SolutionType);
			for (int i = 0; i < 4; i++)
			{
				stringBuilder.AppendFormat("{0,2:f0}  x={1,6:f1}, y={2,6:f1}\r\n", i - 1, Offset_RA_mas[i], Offset_Dec_mas[i]);
			}
			stringBuilder.AppendLine(string.Format("Uncertainty  dRA={0,4:f1}, dDec={1,4:f1}", UncertaintyInX_mas, UncertaintyInY_mas));
			stringBuilder.AppendLine();
			return stringBuilder.ToString();
		}
	}
}
