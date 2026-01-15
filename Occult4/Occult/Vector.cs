namespace Occult
{
	public class Vector
	{
		private double x;

		private double y;

		private double z;

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

		public void SetVectorValues(double A, double B, double C)
		{
			x = A;
			y = B;
			z = C;
		}

		public override string ToString()
		{
			return string.Format("{0,6:f6}, {1,6:f6}, {2,6:f6}", X, Y, Z);
		}
	}
}
