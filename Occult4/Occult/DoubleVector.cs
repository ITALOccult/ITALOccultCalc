namespace Occult
{
	public class DoubleVector
	{
		private double x;

		private double y;

		private double z;

		private double x2;

		private double y2;

		private double z2;

		public double X
		{
			get
			{
				return x;
			}
			set
			{
				x = value;
			}
		}

		public double Y
		{
			get
			{
				return y;
			}
			set
			{
				y = value;
			}
		}

		public double Z
		{
			get
			{
				return z;
			}
			set
			{
				z = value;
			}
		}

		public double X2
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

		public double Y2
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

		public double Z2
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

		public void SetVectorValues(double A, double B, double C)
		{
			x = A;
			y = B;
			z = C;
		}

		public void Set2ndVectorValues(double A, double B, double C)
		{
			x2 = A;
			y2 = B;
			z2 = C;
		}

		public override string ToString()
		{
			return string.Format("{0,6:f6}, {1,6:f6}, {2,6:f6}", X, Y, Z);
		}
	}
}
