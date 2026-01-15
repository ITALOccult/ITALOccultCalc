namespace Occult
{
	public class Matrix
	{
		private double x1;

		private double x2;

		private double x3;

		private double y1;

		private double y2;

		private double y3;

		private double z1;

		private double z2;

		private double z3;

		public double R1C1
		{
			get
			{
				return x1;
			}
			set
			{
				x1 = value;
			}
		}

		public double R1C2
		{
			get
			{
				return x2;
			}
			set
			{
				x2 = value;
			}
		}

		public double R1C3
		{
			get
			{
				return x3;
			}
			set
			{
				x3 = value;
			}
		}

		public double R2C1
		{
			get
			{
				return y1;
			}
			set
			{
				y1 = value;
			}
		}

		public double R2C2
		{
			get
			{
				return y2;
			}
			set
			{
				y2 = value;
			}
		}

		public double R2C3
		{
			get
			{
				return y3;
			}
			set
			{
				y3 = value;
			}
		}

		public double R3C1
		{
			get
			{
				return z1;
			}
			set
			{
				z1 = value;
			}
		}

		public double R3C2
		{
			get
			{
				return z2;
			}
			set
			{
				z2 = value;
			}
		}

		public double R3C3
		{
			get
			{
				return z3;
			}
			set
			{
				z3 = value;
			}
		}

		public void SetMatrixValues(double r1c1, double r1c2, double r1c3, double r2c1, double r2c2, double r2c3, double r3c1, double r3c2, double r3c3)
		{
			x1 = r1c1;
			x2 = r1c2;
			x3 = r1c3;
			y1 = r2c1;
			y2 = r2c2;
			y3 = r2c3;
			z1 = r3c1;
			z2 = r3c2;
			z3 = r3c3;
		}
	}
}
